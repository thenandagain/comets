(ns thenandagain.comets.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]))

(declare comets main-screen)

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (label "Hello world!" (color :white)))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-key-down
  (fn [screen entites]
    (cond
      (key-pressed? :r)
      (on-gl (set-screen! comets main-screen)))

    ))

(defgame comets
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
