(ns storage
  (:use global util))

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
      (guard (not (contains? @objects id))
	     "may not store the same id twice!!")
      (alter objects assoc id object)
      (long-task (run-store-hooks id))
      id)))

(defn unstore [id]
  (dosync 
   (alter objects dissoc id)
   (long-task (run-unstore-hooks id))))

(defn revise [id new-object]
  (dosync 
   (guard (contains? @objects id)
	  "id does not exist!")
   (alter objects assoc id new-object)
   (long-task (run-update-hooks id))))

(defn revise-in [id ks val]
  (dosync 
   (guard (contains? @objects id)
	  "id does not exist!")
   (alter objects assoc-in (concat [id] ks) val)
   (long-task (run-update-hooks id))))

(defn lookup [id]
  (dosync 
   (guard (contains? @objects id)
	  (str  "lookup failed for | " id))
   (get @objects id)))

(defn lookup-in [id & ks]
  (let [obj (lookup id)]
    (get-in obj ks)))

(defn invoke [id & args]
  (apply (lookup id) args))

(defn invoke-in [[id & ks] & args]
  (let [method (lookup id)]))

(defmacro defstore [id args & body]
  (let [method `(fn [~@args] ~@body)]
    (storage/store (keyword (str id)) (eval method))
    nil))