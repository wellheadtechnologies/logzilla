(ns parser
  (:use util clojure.contrib.trace))

(def input)
(def length)
(def num-chars "-1234567890.")
(def start-symbols ["~A", "~C", "~W", "~P", "~V"])
(def next-char)
(def drop-line)
(def advance)
(def input-empty)

(defstruct _version-header :version :wrap)
(defstruct _descriptor :mnemonic :unit :data :description)
(defstruct _las-file :version-header 
	   :well-header :curve-header 
	   :parameter-header :las-curves)
(defstruct _las_curve :descriptor :data)

(defn to-num [text]
  (read-string text))

(defn to-bool [text]
  (cond 
   (= text "YES") true
   (= text "NO") false
   :else 
   (throw 
    (new RuntimeException 
	 (str "to bool value (" text ") not recognized")))))

(defn skip-leading-whitespace []
  (set! input (drop-while #(is-in white-space %) input))
  (when (= (next-char) \#)
    (drop-line) 
    (recur)))

(defmacro save-excursion [& forms]
  `(let [old-input# input
	 result# (do ~@forms)]
     (set! input old-input#)
     result#))

(defn starting-substring? [target source]
    (= (seq target) (take (count target) source)))

(defn skip [target]
  (skip-leading-whitespace)
  (when (starting-substring? target input)
    (do (set! input (drop (count target) input)) nil)))

(defn skip-to [target]
  (if (starting-substring? target input)
    (do (set! input (drop (count target) input)) nil)
    (do
      (advance)
      (when (not (input-empty))
	(recur target)))))

(defn input-empty []
  (empty? input))

(defn advance []
  (set! input (drop 1 input)))

(defn next-char []
  (first input))

(defn upto [target]
  (loop [acc ""]
    (if (or (input-empty) (starting-substring? target input))
      acc
      (let [nacc (str acc (next-char))]
	(advance)
	(recur nacc)))))

(defn partition-last [target]
  (loop [acc "" prev "" last false]
    (if (input-empty)
      [(drop-last prev) (drop (count prev) acc)]
      (if (starting-substring? target input)
	(let [prev (str acc (next-char))]
	  (advance)
	  (recur prev prev true))
	(let [acc (str acc (next-char))]
	  (advance)
	  (recur acc prev false))))))

(defn upto-last [target]
  (first (partition-last target)))

(defmacro limit-line [& forms]
  `(let [old-input# input
	 result# (binding [input (upto "\n")] ~@forms)]
     (set! input old-input#)
     result#))

(defmacro with-input [i & forms]
  `(binding [input ~i]
     ~@forms))

(defn zapto [target]
  (let [result (upto target)]
    (advance)
    result))

(defn zapto-last [target]
  (let [result (upto-last target)]
    (advance)
    result))

(defn grab-line []
  (zapto "\n"))

(defn drop-line []
  (skip-to "\n"))

(defn goto-line [target]
  (let [tlen (count target)]
    (loop []
      (skip-leading-whitespace)
      (when (and (not (input-empty)) 
		 (not (seq-eq (take tlen input) target)))
	(drop-line)
	(recur)))))

(defn descriptor []
  (skip-leading-whitespace)
  (when (and (not (input-empty)) (not (is-in start-symbols (seq-to-str (take 2 input)))))
    (let [mnemonic (upto ".")
	  unit (upto " ")
	  line (upto "\n")
	  [data description] (with-input line (partition-last ":"))]
      (struct-map _descriptor
	:mnemonic (read-seq (trim mnemonic))
	:unit (read-seq (trim unit))
	:data (read-seq (trim data))
	:description (read-seq (trim description))))))

(defn descriptors []
  (loop [ds []]
    (let [d (descriptor)]
      (if (not d)
	ds
	(recur (conj ds d))))))
       
(defn data []
  (map #(to-num (seq-to-str %))
       (loop [nums []]
	 (skip-leading-whitespace)
	 (let [[num,_rest] (split-with #(is-in num-chars %) input)]
	   (set! input _rest)
	   (if (input-empty)
	     (if (empty? num) nums (conj nums num))
	     (recur (if (empty? num) nums (conj nums num))))))))

(defn header [type descriptors]
  {:type type :descriptors descriptors})

(defn well-header []
  (goto-line "~W")
  (drop-line)
  (header :well-header (descriptors)))

(defn version-header []
  (goto-line "~V")
  (drop-line)
  (skip "VERS.")
  (struct-map _version-header
    :version (do (skip "VERS.")
		 (to-num (seq-to-str (upto ":"))))
    :wrap (do (skip "WRAP.")
	      (to-bool (seq-to-str (upto ":"))))))

(defn curve-header []
  (goto-line "~C")
  (drop-line)
  (header :curve-header (descriptors)))

(defn parameter-header []
  (goto-line "~P")
  (drop-line)
  (header :parameter-header (descriptors)))

(defn las-data []
  (goto-line "~A")
  (drop-line)
  (data))

(defn las-curves [curve-header]
  (let [ds (:descriptors curve-header)
	n (count ds)
	data (las-data)
	rows (partition n data)]
    (for [i (range n)]
      (assoc (nth ds i) :data (map #(nth % i) rows)))))
    
(defn las-file []
  (let [vh (save-excursion (version-header))
	wh (save-excursion (well-header))
	ch (save-excursion (curve-header))
	ph (save-excursion (parameter-header))
	lc (save-excursion (las-curves ch))]
    (struct _las-file vh wh ch ph lc)))