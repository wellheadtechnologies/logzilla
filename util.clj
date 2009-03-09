(ns util)

(def white-space "\n\r\t ")

(defn add-value [dataset n comp1 comp2]
  (let [meth (.. dataset (getClass)
		 (getDeclaredMethod "addValue" 
				    (into-array [Number Comparable Comparable])))]
    (. meth (invoke dataset (into-array Object [n comp1 comp2])))))

(defn add-values [dataset & tuples]
  (doseq [tuple tuples]
    (let [[n comp1 comp2] tuple]
      (add-value dataset n comp1 comp2))))

(defn is-in [coll x]
  (some #(= x %) coll))

(defn print-seq [ss]
  (doseq [s ss]
    (print s)))

(defn seq-to-str [string-seq]
  (loop [acc "" ss string-seq]
    (if (empty? ss)
      acc
      (recur (str acc (first ss)) (rest ss)))))

(defn trim [string]
  (let [drop-ws (fn [s] (drop-while #(is-in white-space %) s))]
    (reverse (drop-ws (reverse (drop-ws string))))))

(defn seq-eq [a b]
  (= (trim (seq a)) (trim (seq b))))

(defn read-seq [s]
  (when (not (empty? s))
    (read-string (str \" (seq-to-str s) \"))))

(defn find-first [pred coll]
  (first (filter pred coll)))

