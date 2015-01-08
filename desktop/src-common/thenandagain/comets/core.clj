(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :as math]
            [thenandagain.comets.utils :as utils])
  (:import  [com.badlogic.gdx.math Circle Polygon Vector2]))

(declare comets main-screen)

(defn debug-print [entities]
  (println entities)
  entities)


(defn hitbox->shape [hb]
  (assoc (condp = (type hb)
           Circle (assoc (shape :line
                         :set-color (color :red)
                         :circle 0 0 (. hb radius))
                         :x (. hb x)
                         :y (. hb y))
           Polygon (assoc (shape :line
                          :set-color (color :red)
                          :polygon (math/polygon! hb :get-vertices))
                          :angle (math/polygon! hb :get-rotation)
                          :x (math/polygon! hb :get-x)
                          :y (math/polygon! hb :get-y))
           )
         :hitbox-graphic? true))

(defn directional-vector [m angle]
  (let [rads (Math/toRadians angle)]
    (math/vector-2 (* m (Math/cos rads)) (* m (Math/sin rads)))))

(defn speed-to-angle [v]
  (. v angle))

(defn magnitude [v])

(defn update-position [e]
  (if-let [speed (:speed e)]
    (-> e
        (assoc :x (+ (:x e) (x speed)))
        (assoc :y (+ (:y e) (y speed))))
    e))

(def player-controls
  [(fn accelerate [p entities]
     (if (key-pressed? :dpad-up)
       (let [v (directional-vector (:thrust p)
                                   (:angle p))]

         (assoc p :speed (math/vector-2 0 0
                                        :add (:speed p)
                                        :add v
                                        )))))

   (fn rotate-left [p entities]
     (if (key-pressed? :dpad-left)
       (assoc p :angle (+ (:angle p) 5))))

   (fn rotate-right [p entities]
     (if (key-pressed? :dpad-right)
       (assoc p :angle (- (:angle p) 5))))
   ])

(defn process-controls [entities]
  (map (fn [e]
         (if (:player? e)
           (reduce (fn [p update]
                     (if-let [new-p (update p entities)]
                       new-p
                       p))
                   e
                   player-controls)
           e))
       entities))

(defn generate-a-comet
  ([] (generate-a-comet {}))
  ([opts]
   (let [x (get opts :x (rand (game :width))) ; get screen width?
         y (get opts :y (rand (game :height))) ; get screen height?
         radius (get opts :radius 30.0)
         comet-color (get opts :color (color :white))
         speed (get opts :speed (math/vector-2 (- 1 (rand 2)) (- 1 (rand 2))))
         comet (assoc (shape :line
                             :set-color comet-color
                             :circle 0.0 0.0 radius)
                      :hitbox (math/circle x y radius)
                      :radius radius
                      :x x
                      :y y
                      :speed speed
                      :comet? true)]
     (assoc comet :hitbox-graphic (hitbox->shape (:hitbox comet))))))

(defn get-player [entities]
  (some #(if (:player? %) %) entities))

(defn get-comets [entities]
  (filter :comet? entities))

(defn collision-test-fn [e1 e2]
  (let [ce1 (class (:hitbox e1))
        ce2 (class (:hitbox e2))]
    (cond
      (and (= Circle ce1) (= Circle ce2))
      (fn circle-overlaps-circle [e1 e2]
        (utils/circle-overlaps-circle (:hitbox e1) (:hitbox e2)))

      (and (= Polygon ce1) (= Circle ce2))
      (fn polygon-overlaps-circle [e1 e2]
        (utils/polygon-overlaps-circle (:hitbox e1) (:hitbox e2)))

      (and (= Circle ce1) (= Polygon ce2))
      (fn circle-overlaps-polygon [e1 e2]
        (utils/polygon-overlaps-circle (:hitbox e2) (:hitbox e1)))

      :else
      (fn always-false [e1 e2] false))))

(defn collides-with [e1 e2]
  (let [test-fn (collision-test-fn e1 e2)]
    (test-fn e1 e2)))

(defn find-collisions-with [t entities]
  (let [c (filter (partial collides-with t) entities)]
    c))

(defn find-collisions [entities]
  (let [collidable (filter :hitbox entities)]
    (loop [collisions []
           current (first collidable)
           remaining (rest collidable)]
      (if (not (empty? remaining))
        (recur
          (concat collisions (map (fn [c] [current c]) (find-collisions-with current remaining)))
          (first remaining)
          (rest remaining))
        collisions))))

(defn collision-involves [f]
  (fn [entities] (some f entities)))

(def comet-involved? (collision-involves :comet?))
(def player-involved? (collision-involves :player?))
(def shot-involved? (collision-involves :shot?))

(defn player-death? [collisions]
  (->> collisions
       (filter player-involved?)
       (some comet-involved?)
       not
       not
       ))

(defn select-from-collisions [f collisions]
  (->> collisions
       (apply concat)
       (filter f)))

(defn clone-and-rotate [v d]
  (doto (new Vector2 v)
    (.rotate d)
    (.scl 1.2 1.2)))

(defn split-comet [c]
  (let [angle (speed-to-angle (:speed c))
        new-radius (* 0.75 (:radius c))
        new-speeds (map (partial clone-and-rotate (:speed c)) [20 -20])]
    (map (fn [speed] (generate-a-comet {:speed speed
                                        :x (:x c)
                                        :y (:y c)
                                        :radius new-radius})) new-speeds)))

(defn split-shot-comets [comet-shot-collisions entities]
  (concat entities (->> comet-shot-collisions
                        (select-from-collisions :comet?)
                        (mapcat split-comet))))

(defn remove-shot-comets [comet-shot-collisions entities]
  (let [comet-set (->> comet-shot-collisions
                       (select-from-collisions :comet?)
                       set)]
    (remove
      #(or (comet-set %) (comet-set (:mirror %)))
      entities)))

(defn remove-used-shots [comet-shot-collisions entities]
  (remove
    (->> (apply concat comet-shot-collisions)
         (filter :shot?)
         set)
    entities))

(defn remove-tiny-comets [entities]
  (remove #(and (:comet? %) (<= (:radius %) 10)) entities))

(defn process-comets-hit [entities collisions]
  (let [comet-shot-collisions (filter #(and (comet-involved? %) (shot-involved? %)) collisions)]
    (if (> (count comet-shot-collisions) 0)
      (->> entities
           (split-shot-comets comet-shot-collisions)
           (remove-shot-comets comet-shot-collisions)
           (remove-used-shots comet-shot-collisions)
           remove-tiny-comets)
      entities)))

(defn collision-processing [entities]
  (let [collisions (find-collisions entities)]
    (cond
      (player-death? collisions)
      (on-gl (set-screen! comets main-screen))

      :else
      (process-comets-hit entities collisions)
      )))

(defn create-player [opts]
  (let [verticies (float-array [10.0 0.0, -10.0 8.0, -10.0 -8.0])
        player (assoc (shape :line
                             :set-color (color :green)
                             :polygon verticies)
                      :hitbox (math/polygon verticies
                                            :set-origin 0 0
                                            :set-position (:x opts) (:y opts))
                      :player? true
                      :x (:x opts)
                      :y (:y opts)
                      :angle (get opts :angle 0)
                      :speed (get opts :speed (math/vector-2 0.1 0.1))
                      :thrust 0.1)]
    (assoc player :hitbox-graphic (hitbox->shape (:hitbox player)))))

(defn generate-a-shot [player]
  (let [radius 3
        sx (:x player)
        sy (:y player)
        speed (:speed player)
        angle (:angle player)
        ]
    (-> (shape :filled
               :set-color (color :green)
               :circle 0 0 radius)
        (assoc :x sx
               :y sy
               :angle angle
               :speed (directional-vector 8 angle)
               :hitbox (math/circle sx sy radius)
               :shot? true)
        ((fn hitbox-graphic [s] (assoc s :hitbox-graphic (hitbox->shape (:hitbox s))))))))

(defn sync-child [p c]
  (condp = (type c)
    Circle (do
             (math/circle! c :set-position (:x p) (:y p))
             c)
    Polygon (do
              (math/polygon! c :set-position (:x p) (:y p))
              (math/polygon! c :set-rotation (:angle p))
              c)
    (assoc c
           :x (:x p)
           :y (:y p)
           :angle (:angle p))))

(defn update-children [entities]
  (map (fn update-children-of-entity [e]
         (reduce (fn update-each-child-key [e k]
                   (if-let [child (get e k)]
                     (assoc e k (sync-child e child))
                     e))
                 e
                 [:hitbox :hitbox-graphic]))
       entities))

(def should-draw-hitboxes (atom false))

(defn draw-hitboxes? [entities]
  (if @should-draw-hitboxes
    (concat entities (->> entities
                          (map :hitbox-graphic)
                          (remove nil?)))
    entities))

(defn bounding-box-size [e]
  (let [hb (:hitbox e)]
    (condp = (type hb)
      Circle (math/vector-2 (. hb radius) (. hb radius))
      Polygon (let [bb (. hb getBoundingRectangle)]
                (math/vector-2 (. bb width) (. bb height)))
      (math/vector-2 0 0))))

(defn should-mirror-x? [e]
  (let [width (-> (bounding-box-size e)
                  x
                  (/ 2))]
    (or (< (- (:x e) width) 0)
        (> (+ (:x e) width) (game :width)))))

(defn should-mirror-y? [e]
  (let [height (-> (bounding-box-size e)
                  y
                  (/ 2))]
    (or (< (- (:y e) height) 0)
        (> (+ (:y e) height) (game :height)))))

(defn should-mirror? [e]
  (or (should-mirror-x? e)
      (should-mirror-y? e)))

(defn off-screen? [e]
  (let [bb-size (bounding-box-size e)
        hw (/ (x bb-size) 1.0)
        hh (/ (y bb-size) 1.0)]
    (or (< (+ (:x e) hw) 0)
        (< (+ (:y e) hh) 0)
        (> (- (:x e) hw) (game :width))
        (> (- (:y e) hh) (game :height)))))

(defn min-minus-max [a1 a2]
  (- (min a1 a2) (max a1 a2)))

(defn clone-comet [c]
  (-> (select-keys c [:x :y :radius :speed])
      generate-a-comet))

(defn mirrored-x [c]
  (let [half-width (-> (bounding-box-size c)
                       x
                       (/ 2))]
    (if (< (:x c) (/ (game :width) 2))
      (+ (game :width) (- (:x c)) half-width)
      (+ (- (game :width)) (:x c) half-width))))

(defn mirrored-y [c]
  (let [half-height (-> (bounding-box-size c)
                       y
                       (/ 2))]
    (if (< (:y c) (/ (game :height) 2))
      (+ (game :height) (- (:y c)) half-height)
      (+ (- (game :height)) (:y c) half-height))))

(defn mirror [e]
  (-> (cond
        (:comet? e) (clone-comet e)
        (:shot? e) (generate-a-shot e)
        (:player? e) (create-player e))
      ((fn [ce]
         (if (should-mirror-x? e)
           (assoc ce :x (mirrored-x e))
           ce)))
      ((fn [ce]
        (if (should-mirror-y? e)
          (assoc ce :y (mirrored-y e))
          ce)))))

(defn wrap-around-mirroring [entities]
  (reduce (fn [es e]
            (cond
              (off-screen? e)
              es

              (should-mirror? e)
              (if (:mirror e)
                (conj es e)
                (let [mc (mirror e)]
                  (conj es (assoc e :mirror mc) (assoc mc :mirror e))))

              :else
              (conj es (dissoc e :mirror))))
          []
          entities)
  )

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage) :camera (orthographic))
    (concat (repeatedly 5 generate-a-comet)
            [(create-player {:x (/ (game :width) 2) :y (/ (game :height) 2)})]))

  :on-render
  (fn [screen entities]
    (clear!)
    (let [updated-entities (->> entities
                                wrap-around-mirroring
                                process-controls
                                (map update-position)
                                update-children
                                collision-processing)]
      (->> updated-entities
           (filter :draw!)
           draw-hitboxes?
           (render! screen))
      updated-entities))

  :on-resize
  (fn  [screen entities]
    )

  :on-key-typed
  (fn [screen entities]
    (if (and (key-pressed? :space) (< (count (filter :shot? entities)) 5))
      (conj entities (generate-a-shot (get-player entities)))
      entities))

  :on-key-down
  (fn [screen entities]
    (cond
      (key-pressed? :r)
      (on-gl (set-screen! comets main-screen))
      (key-pressed? :p)
      (println (get-player entities))
      (key-pressed? :h)
      (reset! should-draw-hitboxes (not @should-draw-hitboxes)))
    entities))

(defgame comets
  :on-create
  (fn [this]
    (set-screen! this main-screen)))


(defscreen blank-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer  (stage))
    (label "Danger Will Robinson! Danger!" (color :red))
    )
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-key-down
  (fn [screen entities]
    (cond
      (key-pressed? :r)
      (on-gl (set-screen! comets main-screen)))))

(set-screen-wrapper! (fn [screen screen-fn]
                       (try (screen-fn)
                            (catch Exception e
                              (.printStackTrace e)
                              (set-screen! comets blank-screen)))))
