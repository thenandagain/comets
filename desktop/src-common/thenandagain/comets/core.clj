(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :as math])
  (:import [java.util UUID]))

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

(defn generate-a-comet []
  (let [id (UUID/randomUUID)
        x-off (rand 200)
        y-off (rand 150)]
           (assoc (shape :line
                         :set-color (color :white)
                         :circle 0.0 0.0 20.0)
                  :id id
                  :x (+ 200.0 x-off)
                  :y (+ 150.0 y-off)
                  :speed (math/vector-2 (rand 1.5) (rand 1.5))
                  :angle (rand 360)
                  :comet? true)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (concat (repeatedly 10 generate-a-comet)
            [(assoc (shape :line
                           :set-color (color :green)
                           :triangle 0 10, 8 -10, -8 -10)
                    :player? true
                    :x 100
                    :y 100
                    :angle 270
                    :speed (math/vector-2 0.1 0.1)
                    :thrust 0.1
                    )]))

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
