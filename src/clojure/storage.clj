(ns storage
  (:use global util))

(def objects (ref {})) ; id -> object
(def next-id (ref 0)) ; auto-increment 
(def store-hooks (ref {})) ; id -> [hook]
(def unstore-hooks (ref {})) ; id -> [hook]
(def change-hooks (ref {})) ; [id ks] -> [hook]
(def filters (ref {})) ; [id ks] -> [hook]

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

(defn add-change-hook [id ks hook]
  (dosync 
   (let [hooks (get @change-hooks [id ks] [])]
     (alter change-hooks assoc [id ks] (conj hooks hook)))))

(defn remove-change-hook [id ks hook]
  (dosync
   (let [hooks (get @change-hooks [id ks] [])]
     (alter change-hooks assoc [id ks] (remove #(identical? hook %) hooks)))))

(defn remove-all-change-hooks [id ks]
  (dosync 
   (alter change-hooks dissoc [id ks])))

(defn- run-change-hooks [id ks value]
  (io! 
    (doseq [hook (get @change-hooks [id ks] [])]
      (hook value))))

(defn add-filter [id ks f]
  (dosync 
   (let [fs (get @filters [id ks] [])]
     (alter filters assoc [id ks] (conj fs f)))))

(defn remove-filter [id ks f]
  (dosync
   (let [fs (get @filters [id ks] [])]
     (alter filters assoc [id ks] (remove #(identical? f %) fs)))))

(defn- run-filters [id ks value]
  (loop [fs (get @filters [id ks]), value value]
    (let [f (first fs)]
      (if f
	(recur (rest fs) (f value))
	value))))

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

(defn change [id new-object]
  (dosync 
   (guard (contains? @objects id)
	  "id does not exist!")
   (let [fvalue (run-filters id [] new-object)]
     (alter objects assoc id fvalue)
     (long-task (run-change-hooks id [] fvalue)))))

(defn change-in [id ks val]
  (dosync 
   (guard (contains? @objects id)
	  "id does not exist!")
   (let [fvalue (run-filters id ks val)]
     (alter objects assoc-in (concat [id] ks) fvalue)
     (long-task (run-change-hooks id ks fvalue)))))

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

(defmacro def-change-hook [id ks & body]
  `(storage/add-change-hook ~id ~ks (fn [] ~@body)))

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
       (let [hook (:on-change v)
	     filter (:filter v)]
	 (when hook
	   (add-change-hook root [k] hook))
	 (when filter
	   (add-filter root [k] filter)))))
    properties))

(defn unstore-properties [root]
  (dosync 
   (let [properties (lookup root)
	 meta-data (:meta-data properties)]
     (doseq [k (keys (dissoc properties :meta-data))]
       (remove-all-change-hooks root [k])))
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

(def on-change add-change-hook)