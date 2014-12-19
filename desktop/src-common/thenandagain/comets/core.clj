(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]))

(declare comets main-screen)

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(shape :line
            :set-color (color :green)
            :arc 100 100 20 240 60)])
  
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-key-down
  (fn [screen entites]
    (cond
      (key-pressed? :r)
      (on-gl (set-screen! comets main-screen)))))

(defgame comets
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
