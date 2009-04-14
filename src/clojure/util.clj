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
  (let [c (count (take-while #(not= obj %) coll))]
    (if (not= c (count coll))
      c 
      -1)))

(defn all-same [coll]
  (let [example (first coll)]
    (not (some #(not= example %) coll))))

(defn all-samef [coll]
  (let [example (first coll)]
    (not (some #(> (- example %) 0.00001) coll))))

(defn exec [command]
  (let [runtime (Runtime/getRuntime)
	proc (.exec runtime command)]
    proc))

(defn with-limit [limit x]
  (if (> x limit)
    limit
    x))

(defmacro unless [cond & body]
  `(when (not cond)
     ~@body))

(defn large-to-small [coll] (reverse (sort coll)))

(defn small-to-large [coll] (sort coll))

(defn abs [x]
  (if (< x 0)
    (* -1 x)
    x))

(defn round [x]
  (let [m (mod x 1)
	rounded (- x m)]
    (if (>= m 0.5)
      (inc rounded)
      rounded)))

(defmacro suppress [& body]
  `(do
     ~@body
     nil))

(defmacro standard-imports []
  `(use ~(quote app.controller)))

(defn only [coll]
  (guard (= 1 (count coll))
	 "Collection must have only one member!")
  (first coll))

(defn sqrt [x] (Math/sqrt x))

(defn pow [x y]
  (Math/pow x y))

