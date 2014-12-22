(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :as math]))

(declare comets main-screen)

(defn directional-vector [m angle]
  (let [rads (Math/toRadians angle)]
    (math/vector-2 (* m (Math/cos rads)) (* m (Math/sin rads)))))

(defn update-position [e]
  (-> e
      (assoc :x (+ (:x e) (x (:speed e))))
      (assoc :y (+ (:y e) (y (:speed e))))))

(def player-updates
  [
   (fn accelerate [e]
     (if (key-pressed? :dpad-up)
       (let [v (directional-vector (:thrust e)
                                   (+ 90 (:angle e)))]

         (assoc e :speed (math/vector-2 0 0
                                        :add (:speed e)
                                        :add v
                                        )))))

   (fn rotate-left [e]
     (if (key-pressed? :dpad-left)
       (assoc e :angle (+ (:angle e) 5))))

   (fn rotate-right [e]
     (if (key-pressed? :dpad-right)
       (assoc e :angle (- (:angle e) 5))))
   ])

(defn update-player [entities]
  (map (fn [e]
         (if (:player? e)
           (reduce (fn [p update]
                     (if-let [new-p (update p)]
                       new-p
                       p))
                   e
                   player-updates)
           e))
       entities))



(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(assoc (shape :line
                   :set-color (color :green)
                   :arc 0 10 20 240 60)
            :player? true
            :x 100
            :y 100
            :angle 270
            :speed (math/vector-2 0.1 0.1)
            :thrust 0.1
            )])

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         update-player
         (map update-position)
         (render! screen)))

  :on-key-down
  (fn [screen entities]
    (cond
      (key-pressed? :r)
      (on-gl (set-screen! comets main-screen)))))

(defgame comets
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
