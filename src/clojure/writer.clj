(ns writer
  (:use util las))

(import '(java.io File FileWriter BufferedWriter))

(load "parser")
(refer 'private-parser)
(refer 'parser)

(def writer)

(defn write-data [lasfile]
  (.write writer "~Ascii \n")
  (let [curves (conj (:curves lasfile) (:index lasfile))
	columns (count curves)
	rows (count (:data (first curves)))
	row-data (fn [r] (map #(format "%1.20f " (bigdec (nth (:data %) r))) curves))]
    (doseq [r (range 0 rows)]
      (doto writer
	(.write (seq-to-str (row-data r)))
	(.newLine)))))		

(defn write-descriptor [descriptor]
  (doto writer
    (.write (str (:mnemonic descriptor) " ."))
    (.write (str (:unit descriptor) " "))
    (.write (str (:data descriptor) " : "))
    (.write (str (:description descriptor)))
    (.newLine)))

(defn write-header [header]
  (let [type (:type header)
	descriptors (:descriptors header)
	prefix (get header-prefixes type)]
    (doto writer
      (.write (str prefix))
      (.newLine))
    (doseq [descriptor descriptors]
      (write-descriptor descriptor))))

(defn write-headers [lasfile]
  (let [headers [:version-header
		 :well-header
		 :curve-header
		 :parameter-header]]
    (doseq [h headers]
      (write-header (get lasfile h)))))

(defmulti write-las-file (fn [x y] (class x)))

(defmethod write-las-file File [file-handle lasfile] 
  (let [file-writer (new BufferedWriter (new FileWriter file-handle))]
    (binding [writer file-writer]
      (write-headers lasfile)
      (write-data lasfile))
    (. file-writer (close))))

(defmethod write-las-file String [path lasfile]
  (let [file-handle (new File path)]
    (write-las-file file-handle lasfile)))