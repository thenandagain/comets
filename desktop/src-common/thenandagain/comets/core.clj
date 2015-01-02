(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :as math]
            [thenandagain.comets.utils :as utils])
  (:import [java.util UUID]
           [com.badlogic.gdx.math Circle Polygon]))

(declare comets main-screen)

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
                                   (+ 90 (:angle p)))]

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
  (let [id (UUID/randomUUID)
        x-off (rand 200)
        y-off (rand 150)
        x (+ 200.0 x-off)
        y (+ 150.0 y-off)
        radius 30.0]
           (assoc (shape :line
                         :set-color (color :white)
                         :circle 0.0 0.0 radius)
                  :id id
                  :hitbox (math/circle x y radius)
                  :x x
                  :y y
                  :speed (math/vector-2 (- 1.5 (rand 3)) (- 1.5 (rand 3)))
                  :angle (rand 360)
                  :comet? true)))

(defn get-player [entities]
  (some #(if (:player? %) %) entities))

(defn get-comets [entities]
  (filter :comet? entities))

(defn collision-processing [entities]
  (let [p (:hitbox (get-player entities))
        collisions (->> entities
                       get-comets
                       (map :hitbox)
                       (filter (partial utils/polygon-overlaps-circle p)))]
    (if (> (count collisions) 0)
      (on-gl (set-screen! comets main-screen))
      entities)))

(defn create-player [x y]
  (let [verticies (float-array [0.0 10.0, 8.0 -10.0, -8.0 -10.0])]
    (assoc (shape :line
                  :set-color (color :green)
                  :polygon verticies)
           :hitbox (math/polygon verticies
                                 :set-origin 0 0
                                 :set-rotation 270
                                 :set-position x y)
           :player? true
           :x x
           :y y
           :angle 270
           :speed (math/vector-2 0.1 0.1)
           :thrust 0.1
           )))

(defn update-hitbox [e hb]
  (condp = (type hb)
    Circle (do
             (math/circle! hb :set-position (:x e) (:y e)))
    Polygon (do
              (math/polygon! hb :set-position (:x e) (:y e))
              (math/polygon! hb :set-rotation (:angle e))))
  e
  )

(defn update-hitboxes! [entities]
  (map (fn [e]
         (if-let [hb (:hitbox e)]
           (update-hitbox e hb))
         e)
       entities))

(defn hitbox->shape [hb]
  (assoc (condp = (type hb)
           Circle (shape :line
                         :set-color (color :red)
                         :circle (. hb x) (. hb y) (. hb radius))
           Polygon (assoc (shape :line
                          :set-color (color :red)
                          :polygon (math/polygon! hb :get-vertices))
                          :angle (math/polygon! hb :get-rotation)
                          :x (math/polygon! hb :get-x)
                          :y (math/polygon! hb :get-y))
           )
         :hitbox? true)
  )

(def should-draw-hitboxes (atom false))

(defn generate-hitbox-graphics [entities]
  (->> entities
       (filter :hitbox)
       (map :hitbox)
       (map hitbox->shape)))

(defn draw-hitboxes? [entities]
  (let [es (remove :hitbox? entities)]
    (if @should-draw-hitboxes
      (concat es (generate-hitbox-graphics es))
      es)))

(defn debug-print [entities]
  (println entities)
  entities)



(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (concat (repeatedly 5 generate-a-comet)
            [(create-player 100.0 100.0)]))

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         process-controls
         (map update-position)
         update-hitboxes!
         draw-hitboxes?
         collision-processing
         (render! screen)))

  :on-key-down
  (fn [screen entities]
    (cond
      (key-pressed? :r)
      (on-gl (set-screen! comets main-screen))
      (key-pressed? :p)
      (println (get-player entities)))))

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
