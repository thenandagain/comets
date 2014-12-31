(defproject thenandagain/comets "0.0.1-SNAPSHOT"
  :description "An Asteroids clone, with a twist?"
  
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.badlogicgames.gdx/gdx "1.5.0"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.5.0"]
                 [com.badlogicgames.gdx/gdx-box2d "1.5.0"]
                 [com.badlogicgames.gdx/gdx-box2d-platform "1.5.0"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-bullet "1.5.0"]
                 [com.badlogicgames.gdx/gdx-bullet-platform "1.5.0"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-platform "1.5.0"
                  :classifier "natives-desktop"]
                 [play-clj "0.4.3"]]
  
  :source-paths ["src" "src-common"]
  :test-paths ["test-common"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [thenandagain.comets.core.desktop-launcher]
  :main thenandagain.comets.core.desktop-launcher)
