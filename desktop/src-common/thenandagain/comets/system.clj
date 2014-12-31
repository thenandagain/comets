(ns thenandagain.comets.system
  (:import [java.util UUID]))

(defn conj-id-onto-keys [m ks id]
  (reduce (fn [new-m k]
            (update-in new-m [k]
                       #(if %
                          (conj % id)
                          [id])))
          m
          ks))

(defprotocol IEntityComponentSystem
  (add-component  [system e-id k v])
  (get-components [system e-id])
  (get-component  [system e-id k] [system e-id k not-found])

  (entities-with-component [system k])
  (update-component [system e-id k f & args]))

(defn create-entity []
  (UUID/randomUUID))

(defrecord EntityComponentSystem [entity->components component->entities]
  IEntityComponentSystem
  (add-component [system e-id k v]
    (->EntityComponentSystem
      (assoc-in entity->components [e-id k] v)
      (update-in component->entities [k] #(if % (conj % e-id) [e-id]))))

  (get-components [_ e-id]
    (get entity->components e-id))

  (get-component [s e-id k]
    (get-component s e-id k nil))

  (get-component [_ e-id k not-found]
    (get-in entity->components [e-id k] not-found))

  (entities-with-component [_ k]
    (get component->entities k)))

(defn update-component [system e-id k f & args]
  (add-component system
                 e-id
                 k
                 (apply f (get-component system e-id k) args)))

(defn create-system []
  (->EntityComponentSystem {} {}))

