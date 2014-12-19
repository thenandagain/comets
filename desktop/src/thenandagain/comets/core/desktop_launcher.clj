(ns thenandagain.comets.core.desktop-launcher
  (:require [thenandagain.comets.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. comets "thenandagain/comets" 800 600)
  (Keyboard/enableRepeatEvents true))
