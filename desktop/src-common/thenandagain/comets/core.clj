(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer :all]))

(declare comets main-screen)

(defn directional-vector [m angle]
  (let [rads (Math/toRadians angle)]
    {:dx (* m (Math/cos rads)) :dy (* m (Math/sin rads))}))

(defn update-position [e]
  (-> e
      (assoc :x (+ (:x e) (get-in e [:speed :dx] 0)))
      (assoc :y (+ (:y e) (get-in e [:speed :dy] 0)))))

(defn update-player [entities]
  (map (fn [e]
         (if (:player? e)
           (-> e
               ((fn [e] (cond
                          ; with this implementation, only one key will be 
                          ; considered pressed at any one time. needs to be
                          ; redone
                          (key-pressed? :dpad-up)
                          (let [v (directional-vector (:acceleration e)
                                                      (+ 90 (:angle e)))]

                            (update-in e [:speed] (fn [s]
                                                  {:dx (+ (:dx s) (:dx v))
                                                   :dy (+ (:dy s) (:dy v))})))

                          (key-pressed? :dpad-left)
                          (assoc e :angle (+ (:angle e) 5))

                          (key-pressed? :dpad-right)
                          (assoc e :angle (- (:angle e) 5))

                          :defalt
                          e)))
                update-position)
           e)) entities))

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
            :speed {:dx 0.1, :dy 0.1}
            :acceleration 0.1
            )])

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (update-player)
         (render! screen)))

  :on-key-down
  (fn [screen entites]
    (cond
      (key-pressed? :r)
      (on-gl (set-screen! comets main-screen)))))

(defgame comets
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
