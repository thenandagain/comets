(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer :all]))

(declare comets main-screen)

(defn update-player [entities]
  (map (fn [e]
         (if (:player? e)
           (-> e
               ((fn [e] (cond
                          ; with this implementation, only one key will be 
                          ; considered pressed at any one time. needs to be
                          ; redone
                          (key-pressed? :dpad-up)
                          (assoc e :speed (+ (:speed e) (:acceleration e)))

                          (key-pressed? :dpad-down)
                          (assoc e :speed (- (:speed e) (:acceleration e)))

                          ; Currently rotates around the point of the arc,
                          ; not the 'center'.
                          (key-pressed? :dpad-left)
                          (assoc e :angle (+ (:angle e) 5))

                          (key-pressed? :dpad-right)
                          (assoc e :angle (- (:angle e) 5))

                          :defalt
                          e)))
               (assoc :x (+ (:x e) (:speed e 0))))
           e)) entities))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(assoc (shape :line
                   :set-color (color :green)
                   :arc 0 0 20 240 60)
            :player? true
            :x 100
            :y 100
            :angle 0
            :speed 0.1
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
