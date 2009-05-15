(ns messages
  (:use util global))

(def disabled [])

(deflogger messages)

(defmacro ignore [type & body]
  `(binding [disabled (conj disabled ~type)]
     ~@body))

(defn fire [type src event]
  (when (not-any? #(= type %) disabled)
    (info (str "fire " type))
    (let [listeners (get-in @src [:listeners type])]
      (short-task
       (doseq [{:keys [destination callback]} listeners]
	 (when (not= destination (:source event))
	   (callback event)))))))

(defn receive [type obj event]
  (when (not-any? #(= type %) disabled)
    (let [receivers (get-in @obj [:receivers type])]
      (short-task
       (doseq [receiver receivers]
	 (when (not= obj (:source event))
	   (receiver event)))))))

(defn add-listener [type src dst callback]
  (dosync
   (let [old-listeners (get-in @src [:listeners type])
	 listener {:destination dst :callback callback}]
     (alter src assoc-in [:listeners type] (conj old-listeners listener)))))

(defn remove-listener [type src dst callback]
  (dosync
   (let [old-listeners (get-in @src [:listeners type])
	 listener {:destination dst :callback callback}]
     (alter src assoc-in [:listeners type] (remove #(= listener %) old-listeners)))))


(defn add-receiver [type obj receiver]
  (dosync
   (let [old-receivers (get-in @obj [:receivers type])]
     (alter obj assoc-in [:receivers type] (conj old-receivers receiver)))))

(defn remove-receiver [type obj receiver]
  (dosync
   (let [old-receivers (get-in @obj [:receivers type])]
     (alter obj assoc-in [:receivers type] (remove #(= receiver %) old-receivers)))))

