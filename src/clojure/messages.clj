(ns messages
  (:use util global))

(def disabled [])

(defmacro ignore [type & body]
  `(binding [disabled (conj disabled ~type)]
     ~@body))

(defn fire [type object event]
  (when (not-any? #(= type %) disabled)
    (let [listeners (get-in @object [:listeners type])]
      (short-task
       (doseq [listener listeners]
	 (listener event))))))


(defn add-listener [type object listener]
  (dosync
   (let [old-listeners (get-in @object [:listeners type])]
     (alter object assoc-in [:listeners type] (conj old-listeners listener)))))

(defn remove-listener [type object listener]
  (dosync
   (let [old-listeners (get-in @object [:listeners type])]
     (alter object assoc-in [:listeners type] (remove #(= listener %) old-listeners)))))