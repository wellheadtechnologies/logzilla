(ns storage
  (:use global))

(def objects (ref {})) ; id -> object
(def next-id (ref 0)) ; auto-increment 
(def store-hooks (ref {})) ; id -> [hook]
(def unstore-hooks (ref [])) ; id -> [hook]
(def update-hooks (ref [])) ; id -> [hook]

(defn add-store-hook [id hook]
  (dosync 
   (let [hooks (get @store-hooks id [])]
     (alter store-hooks assoc id (conj hooks hook)))))

(defn remove-store-hook [id hook]
  (dosync 
   (let [hooks (get @store-hooks id [])]
     (alter store-hooks assoc id (remove #(identical? hook %) hooks)))))

(defn- run-store-hooks [id]
  (doseq [hook (get @store-hooks id [])]
    (hook)))

(defn add-unstore-hook [id hook]
  (dosync 
   (let [hooks (get @unstore-hooks id [])]
     (alter unstore-hooks assoc id (conj hooks hook)))))

(defn remove-unstore-hook [id hook]
  (dosync 
   (let [hooks (get @unstore-hooks id [])]
     (alter unstore-hooks assoc id (remove #(identical? hook %) hooks)))))

(defn- run-unstore-hooks [id]
  (doseq [hook (get @unstore-hooks id [])]
    (hook)))

(defn add-update-hook [id hook]
  (dosync 
   (let [hooks (get @update-hooks id [])]
     (alter update-hooks assoc id (conj hooks hook)))))

(defn remove-update-hook [id hook]
  (dosync
   (let [hooks (get @update-hooks id [])]
     (alter update-hooks assoc id (remove #(identical? hook %) hooks)))))

(defn- run-update-hooks [id]
  (doseq [hook (get @update-hooks id [])]
    (hook)))

(defn store
  ([object]
     (dosync 
      (let [id @next-id]
	(alter objects assoc id object)
	(alter next-id inc)
	(long-task (run-store-hooks id))
	id)))
  ([id object]
     (dosync 
      (alter objects assoc id object)
      (long-task (run-store-hooks id))
      id)))

(defn unstore [id]
  (dosync 
   (alter objects dissoc id)
   (long-task (run-unstore-hooks id))))

(defn update [id new-object]
  (dosync 
   (alter objects assoc id new-object)
   (long-task (run-update-hooks id))))

(defn lookup [id]
  (get @objects id))

(defn invoke [id & args]
  (apply (lookup id) args))

(defmacro defstore [id args & body]
  `(storage/store ~id (fn [~@args] ~@body)))