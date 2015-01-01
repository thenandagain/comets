(ns thenandagain.comets.system
  (:import [java.util UUID]))

(defprotocol IEntityComponentSystem
  (add-component  [system e-id c])
  (get-components [system e-id])
  (get-component  [system e-id c] [system e-id k not-found])
  (entities-with-component [system c])
  (all-components [system])
  (remove-component [system e-id c])
  (remove-entity [system e-id]))

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
        (update-in component->entities [k] #(if % (conj % e-id) #{e-id})))))

  (get-components [_ e-id]
    (vals (get entity->components e-id)))

  (get-component [s e-id c]
    (get-component s e-id c nil))

  (get-component [_ e-id c not-found]
    (get-in entity->components [e-id c] not-found))

  (entities-with-component [_ c]
    (get component->entities c))

  (all-components [_]
    (mapcat #(vals (second %)) entity->components))

  (remove-component [_ e-id c]
    (->EntityComponentSystem (update-in entity->components  [e-id] #(dissoc % c))
                             (update-in component->entities [c]    #(disj % e-id))))
  (remove-entity [_ e-id]
    (let [cs (map #(class (second %)) (get entity->components e-id))]
      (->EntityComponentSystem
        (dissoc entity->components e-id)
        (reduce (fn [c->e c]
                  (update-in component->entities [c] #(disj % e-id)))
                component->entities
                cs)))))

(defn update-component [system e-id c f & args]
  (let [new-component (apply f (get-component system e-id c) args)]
    (if (= (class new-component) c)
      (add-component system e-id new-component)
      (throw (Exception.
               (str "Update function expected to return `"
                    c
                    "` but returned `"
                    (class new-component)
                    "`"))))))

(defn add-components [system e-id components]
  (reduce (fn [s c]
            (add-component s e-id c))
          components))

(defn create-system []
  (->EntityComponentSystem {} {}))

