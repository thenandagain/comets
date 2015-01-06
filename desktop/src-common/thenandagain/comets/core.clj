(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :as math]
            [thenandagain.comets.utils :as utils])
  (:import  [com.badlogic.gdx.math Circle Polygon]))

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

(defn generate-a-comet []
  (let [x-off (rand 200)
        y-off (rand 150)
        x (+ 200.0 x-off)
        y (+ 150.0 y-off)
        radius 30.0
        comet (assoc (shape :line
                            :set-color (color :white)
                            :circle 0.0 0.0 radius)
                     :hitbox (math/circle x y radius)
                     :x x
                     :y y
                     :speed (math/vector-2 (- 1 (rand 2)) (- 1 (rand 2)))
                     :angle (rand 360)
                     :comet? true)]
    (assoc comet :hitbox-graphic (hitbox->shape (:hitbox comet)))))

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

(defn split-shot-comets [comet-shot-collisions entities]
  entities)

(defn remove-shot-comets [comet-shot-collisions entities]
  (remove
    (->> (apply concat comet-shot-collisions)
         (filter :comet?)
         set)
    entities))

(defn remove-used-shots [comet-shot-collisions entities]
  (remove
    (->> (apply concat comet-shot-collisions)
         (filter :shot?)
         set)
    entities))


(defn process-comets-hit [entities collisions]
  (let [comet-shot-collisions (filter #(and (comet-involved? %) (shot-involved? %)) collisions)]
    (if (> (count comet-shot-collisions) 0)
      (->> entities
          (split-shot-comets comet-shot-collisions)
          (remove-shot-comets comet-shot-collisions)
          (remove-used-shots comet-shot-collisions))
      entities)))

(defn collision-processing [entities]
  (let [collisions (find-collisions entities)]
    (cond
      (player-death? collisions)
      (on-gl (set-screen! comets main-screen))

      :else
      (process-comets-hit entities collisions)
      )))

(defn create-player [x y]
  (let [verticies (float-array [10.0 0.0, -10.0 8.0, -10.0 -8.0])
        player (assoc (shape :line
                             :set-color (color :green)
                             :polygon verticies)
                      :hitbox (math/polygon verticies
                                            :set-origin 0 0
                                            :set-position x y)
                      :player? true
                      :x x
                      :y y
                      :angle 0
                      :speed (math/vector-2 0.1 0.1)
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

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (concat (repeatedly 5 generate-a-comet)
            [(create-player 100.0 100.0)]))

  :on-render
  (fn [screen entities]
    (clear!)
    (let [updated-entities (->> entities
                                process-controls
                                (map update-position)
                                update-children
                                collision-processing)]
      (->> updated-entities
           (filter :draw!)
           draw-hitboxes?
           (render! screen))
      updated-entities))

  :on-key-down
  (fn [screen entities]
    (if (key-pressed? :space)
      (conj entities (generate-a-shot (get-player entities)))
      (do
        (cond
          (key-pressed? :r)
          (on-gl (set-screen! comets main-screen))
          (key-pressed? :p)
          (println (get-player entities))
          (key-pressed? :h)
          (reset! should-draw-hitboxes (not @should-draw-hitboxes)))
          nil))))

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
