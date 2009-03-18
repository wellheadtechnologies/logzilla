(ns util)

(import '(java.util List LinkedList))

(def white-space "\n\r\t ")

(defn escape-quotes [text]
  (.replaceAll text "\"" "\\\\\""))

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
  (seq-to-str
   (let [drop-ws (fn [s] (drop-while #(is-in white-space %) s))]
     (reverse (drop-ws (reverse (drop-ws string)))))))

(defn seq-eq [a b]
  (= (trim (seq a)) (trim (seq b))))

(defn read-seq [s]
  (when (not (empty? s))
    (read-string (str \" (escape-quotes (seq-to-str s)) \"))))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn tuplize [& colls]
  (let [n (count colls)]
    (partition n (apply interleave colls))))

(defn nil-or? [text]
  (when (not (empty? text))
    text))

(defn to-linked-list [coll]
  (let [ll (new LinkedList)]
    (doseq [c coll]
      (.add ll c))
    ll))

(defn guard [condition msg]
  (when (not condition)
    (throw (new RuntimeException msg))))
    
(defn index-of [obj coll]
  (count (take-while #(not= obj %) coll)))

(defn get-curve [name curves]
  (find-first #(= (.getMnemonic %) name) curves))