(ns storage
  (:use global util))

(def objects (ref {})) ; id -> object
(def next-id (ref 0)) ; auto-increment 
(def store-hooks (ref {})) ; id -> [hook]
(def unstore-hooks (ref {})) ; id -> [hook]
(def revise-hooks (ref {})) ; [id ks] -> [hook]

(defn add-store-hook [id hook]
  (dosync 
   (let [hooks (get @store-hooks id [])]
     (alter store-hooks assoc id (conj hooks hook)))))

(defn remove-store-hook [id hook]
  (dosync 
   (let [hooks (get @store-hooks id [])]
     (alter store-hooks assoc id (remove #(identical? hook %) hooks)))))

(defn- run-store-hooks [id]
  (io! 
   (doseq [hook (get @store-hooks id [])]
     (hook))))

(defn add-unstore-hook [id hook]
  (dosync 
   (let [hooks (get @unstore-hooks id [])]
     (alter unstore-hooks assoc id (conj hooks hook)))))

(defn remove-unstore-hook [id hook]
  (dosync 
   (let [hooks (get @unstore-hooks id [])]
     (alter unstore-hooks assoc id (remove #(identical? hook %) hooks)))))

(defn- run-unstore-hooks [id]
  (io! 
   (doseq [hook (get @unstore-hooks id [])]
     (hook))))

(defn add-revise-hook [id ks hook]
  (dosync 
   (let [hooks (get @revise-hooks [id ks] [])]
     (alter revise-hooks assoc [id ks] (conj hooks hook)))))

(defn remove-revise-hook [id ks hook]
  (dosync
   (let [hooks (get @revise-hooks [id ks] [])]
     (alter revise-hooks assoc [id ks] (remove #(identical? hook %) hooks)))))

(defn remove-all-revise-hooks [id ks]
  (dosync 
   (alter revise-hooks dissoc [id ks])))

(defn- run-revise-hooks [id ks]
  (io! 
   (doseq [hook (get @revise-hooks [id ks] [])]
     (hook))))

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
   (long-task (run-revise-hooks id []))))

(defn revise-in [id ks val]
  (dosync 
   (guard (contains? @objects id)
	  "id does not exist!")
   (alter objects assoc-in (concat [id] ks) val)
   (long-task (run-revise-hooks id ks))))

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
  (let [method (apply lookup-in id ks)]
    (apply method args)))

(defmacro def-revise-hook [id ks & body]
  `(storage/add-revise-hook ~id ~ks (fn [] ~@body)))

(defn instance-properties [& properties]
  (let [keys (map first properties)
	values (map second properties)
	meta-data (map #(drop 2 %) properties)
	properties (apply merge 
			  (for [[k v] (tuplize keys values)]
			    {k v}))
	meta-data (apply merge 
			 (for [[k v] (tuplize keys meta-data)]
			   {k (apply hash-map v)}))]
    (assoc properties :meta-data meta-data)))

(defmacro defproperties [name & properties]
  (list 'def name (apply instance-properties properties)))

(defn store-properties [root properties]
  (let [meta-data (:meta-data properties)]
    (dosync 
     (store root properties)
     (doseq [[k v] meta-data]
       (let [hook (:on-revise v)]
	 (when hook
	   (print-task "add-revise-hook for " k)
	   (add-revise-hook root [k] hook)))))
    properties))

(defn unstore-properties [root]
  (dosync 
   (let [properties (lookup root)
	 meta-data (:meta-data properties)]
     (doseq [k (keys (dissoc properties :meta-data))]
       (remove-all-revise-hooks root [k])))
   (unstore root)))

(defn init-properties [properties]
  (io!
   (let [meta-data (:meta-data properties)
	 properties (dissoc properties :meta-data)
	 inited-properties (apply merge 
				  (for [[k v] properties]
				    (let [m (get meta-data k)
					  init (:init m)]
				      (if (and (nil? v)
					       (contains? m :init))
					{k (init)}
					{k v}))))]
     (assoc inited-properties :meta-data meta-data))))
