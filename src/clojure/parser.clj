(ns private-parser
  (:use util clojure.contrib.trace))

(def input)
(def length)
(def num-chars "-1234567890.")
(def start-symbols ["~A", "~C", "~W", "~P", "~V"])
(def current-char)
(def drop-line)
(def advance)
(def input-empty)

(defstruct version-header :version :wrap)
(defstruct descriptor :mnemonic :unit :data :description)
(defstruct las-file :version-header 
	   :well-header :curve-header 
	   :parameter-header :curves :index)
(defstruct curve :descriptor :data)

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
  (when (= (current-char) \#)
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

(defn current-char []
  (first input))

(defn upto [target]
  (loop [acc ""]
    (if (or (input-empty) (starting-substring? target input))
      (when (not (= acc "")) acc)
      (let [nacc (str acc (current-char))]
	(advance)
	(recur nacc)))))

(defn partition-last [target]
  (loop [acc "" prev "" last false]
    (if (input-empty)
      [(drop-last prev) (drop (count prev) acc)]
      (if (starting-substring? target input)
	(let [prev (str acc (current-char))]
	  (advance)
	  (recur prev prev true))
	(let [acc (str acc (current-char))]
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

(defn goto-drop [target]
  (goto-line target)
  (drop-line))

(defn parse-descriptor []
  (skip-leading-whitespace)
  (when (and (not (input-empty)) (not (is-in start-symbols (seq-to-str (take 2 input)))))
    (let [mnemonic (zapto ".")
	  unit (upto " ")
	  line (upto "\n")
	  [data description] (with-input line (partition-last ":"))]
      (struct-map descriptor
	:mnemonic (read-seq (trim mnemonic))
	:unit (read-seq (trim unit))
	:data (read-seq (trim data))
	:description (read-seq (trim description))))))

(defn parse-descriptors []
  (loop [ds []]
    (let [d (parse-descriptor)]
      (if (not d)
	ds
	(recur (conj ds d))))))
       
(defn parse-data []
  (goto-drop "~A")
  (map #(to-num (seq-to-str %))
       (loop [nums []]
	 (skip-leading-whitespace)
	 (let [[num,_rest] (split-with #(is-in num-chars %) input)]
	   (set! input _rest)
	   (if (input-empty)
	     (if (empty? num) nums (conj nums num))
	     (recur (if (empty? num) nums (conj nums num))))))))

(defn parse-header [type descriptors]
  {:type type :descriptors descriptors})

(defn parse-well-header []
  (goto-drop "~W")
  (parse-header :well-header (parse-descriptors)))

(defn parse-version-header []
  (goto-drop "~V")
  (skip "VERS.")
  (let [version (to-num (trim (zapto ":")))]
    (drop-line)
    (skip "WRAP.")
    (let [wrap (to-bool (trim (zapto ":")))]
      (drop-line)
      (struct version-header version wrap))))

(defn parse-curve-header []
  (goto-drop "~C")
  (parse-header :curve-header (parse-descriptors)))

(defn parse-parameter-header []
  (goto-drop "~P")
  (parse-header :parameter-header (parse-descriptors)))

(defn parse-curves [curve-header]
  (let [ds (:descriptors curve-header)
	n (count ds)
	data (parse-data)
	rows (partition n data)
	curves (for [i (range n)]
		 (assoc (nth ds i) :data (map #(nth % i) rows)))
	index (first curves)]
    [index (map #(assoc % :index index) (rest curves))]))
    
(defn _parse-las-file []
  (let [vh (save-excursion (parse-version-header))
	wh (save-excursion (parse-well-header))
	ch (save-excursion (parse-curve-header))
	ph (save-excursion (parse-parameter-header))
	[index curves] (save-excursion (parse-curves ch))]
    (struct las-file vh wh ch ph curves index)))

(ns parser)
(refer 'private-parser)

(defn parse-las-file [text]
  (with-input text (_parse-las-file)))