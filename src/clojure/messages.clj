(ns messages
  (:use util))

(def listeners (ref {})) ; [object type] -> listeners
(def disabled (ref {})) ; [object type] -> true|false

(defmacro ignore [type object & body]
  `(dosync
    (alter disabled assoc ~[object type] true)
    ~@body
    (alter disabled assoc ~[object type] false)))

(defn fire [type object event]
  (when (not (get @disabled [object type]))
    (dosync
     (let [listeners (get @listeners [object type])]
       (doseq [listener listeners]
	 (listener event))))))

(defn add-listener [type object listener]
  (dosync
   (let [old-listeners (get @listeners [object type])]
     (alter listeners assoc [object type] (conj old-listeners listener)))))

(defn remove-listener [type object listener]
  (dosync
   (let [old-listeners (get @listeners [object type])]
     (alter listeners assoc [object type]
	    (remove #(= listener %) old-listeners)))))