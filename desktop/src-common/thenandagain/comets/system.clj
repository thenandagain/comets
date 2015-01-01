(ns thenandagain.comets.system
  (:import [java.util UUID]))

(defprotocol IEntityComponentSystem
  (add-component  [system e-id c])
  (get-components [system e-id])
  (get-component  [system e-id c] [system e-id k not-found])
  (entities-with-component [system c])
  (all-components [system]))

(defn create-entity []
  (UUID/randomUUID))

(defn component-type [c]
  (class c))

(defn get-owner [component]
  (:e-id (meta component)))

(defrecord EntityComponentSystem [entity->components component->entities]
  IEntityComponentSystem
  (add-component [system e-id c]
    (let [k (component-type c)]
      (->EntityComponentSystem
        (assoc-in entity->components [e-id k] (with-meta c {:e-id e-id}))
        (update-in component->entities [k] #(if % (conj % e-id) [e-id])))))

  (get-components [_ e-id]
    (get entity->components e-id))

  (get-component [s e-id c]
    (get-component s e-id c nil))

  (get-component [_ e-id c not-found]
    (get-in entity->components [e-id c] not-found))

  (entities-with-component [_ c]
    (get component->entities c))

  (all-components [_]
    (concat (vals entity->components))))

(defn update-component [system e-id c f & args]
  (add-component system
                 e-id
                 (apply f (get-component system e-id c) args)))

(defn create-system []
  (->EntityComponentSystem {} {}))

