(ns tparser  
  (:use util data las))

(load "parser")
(refer 'private-parser)
(refer 'parser)

(defn test-zapping []
  (with-input "abc."
    (assert (seq-eq (upto ".") "abc")))
  (with-input well-header-text
    (drop-line)
    (assert (seq-eq (grab-line) "~Well"))
    (goto-line "UWI")
    (assert (seq-eq (grab-line) "UWI.      : UNIQUE WELL ID"))
    (assert (seq-eq (upto ".") "API")))
  (with-input well-header-text
    (goto-line "DATE")
    (assert (seq-eq "DATE" (zapto ".")))
    (assert (seq-eq "" (zapto " ")))
    (assert (seq-eq "Monday, January 26 2009 14:04:02"
		    (limit-line (zapto-last ":"))))))

(defn test-descriptors []
  (let [parse-descriptor (fn [key] (with-input (key descriptors-text) (descriptor)))
	compare-descriptor (fn [a b c d e] 
			     (println "testing " a)
			     (assert (= a (struct _descriptor b c d e))))
	dept (parse-descriptor :dept)
	net-gross (parse-descriptor :net-gross)
	facies (parse-descriptor :facies)
	porosity (parse-descriptor :porosity)
	gamma (parse-descriptor :gamma)
	depth (parse-descriptor :depth)
	start (parse-descriptor :start)
	stop (parse-descriptor :stop)
	date (parse-descriptor :date)]
    (compare-descriptor dept "DEPT" "m" nil "DEPTH")
    (compare-descriptor net-gross "NetGross" nil nil "NetGross")
    (compare-descriptor facies "Facies" nil nil "Facies")
    (compare-descriptor porosity "Porosity" "m3/m3" nil "Porosity")
    (compare-descriptor gamma "Gamma" "gAPI" nil "Gamma")
    (compare-descriptor depth "DEPTH" "m" nil "trend")
    (compare-descriptor start "STRT" "m" "1499.8790000" nil)
    (compare-descriptor stop "STOP" "m" "2416.3790000" nil)
    (compare-descriptor date "DATE" nil "Monday, January 26 2009 14:04:02" "DATE")))

(defn test-version-header []
  (with-input version-header-text
    (assert (= (version-header) {:version 2.0, :wrap false}))))

(defn test-well-header []
  (with-input well-header-text
    (let [wh (well-header)
	  ds (:descriptors wh)
	  date (find-first #(= "DATE" (:mnemonic %)) ds)]
      (assert (= (:data date) "Monday, January 26 2009 14:04:02"))
      (assert (= (:description date) "DATE")))))

(defn test-curve-header []
  (with-input curve-header-text
    (let [ch (curve-header)
	  ds (:descriptors ch)]
      (assert (= 6 (count ds)))
      (assert (every? #(is-in ["DEPT" "NetGross" "Facies" "Porosity" "Gamma" "DEPTH"] %)
		      (map #(:mnemonic %) ds))))))

(def my-curve-header (with-input curve-header-text (curve-header)))

(defn test-las-curves []
  (with-input las-data-text
    (let [curves (las-curves my-curve-header)]
      (assert (= 6 (count curves)))
      (assert (every? #(= 9 (count (:data %))) curves)))))

(defn test-las-file []
  (with-input (slurp "las_files/test.las")
    (let [lf (las-file)
	  dept (get-curve lf "DEPT")
	  gamma (get-curve lf "Gamma")
	  porosity (get-curve lf "Porosity")]
      (assert (= 1501.629 (nth (:data dept) 0)))
      (assert (= "gAPI" (:unit gamma)))
      (assert (= "m3/m3" (:unit porosity)))
      (assert (= "DEPTH" (:description dept))))))

(defn test-dollie []
  (with-input (slurp "las_files/dollie.las")
    (let [lf (las-file)
	  dept (get-curve lf "DEPT")
	  wtoc (get-curve lf "WTOC")]
      (assert (not (nil? dept)))
      (assert (not (nil? wtoc)))
      (assert (= 7800 (nth (:data dept) 0)))
      (assert (= 6680 (last (:data dept))))
      (assert (= "LBF/LBF" (:unit wtoc))))))

(defn test-x4 []
  (with-input (slurp "las_files/x4.las")
    (let [lf (las-file)
	  wh (:well-header lf)
	  strt (:data (get-descriptor wh "STRT"))
	  stop (:data (get-descriptor wh "STOP"))]
      (assert (not (nil? wh)))
      (assert (not (nil? strt)))
      (assert (not (nil? stop)))
      (assert (= strt "57.000000000"))
      (assert (= stop "5817.0000000")))))

(defn run-tests []
  (test-zapping)
  (println "finished zapping")
  (test-descriptors)
  (println "finished descriptors")
  (test-version-header)
  (println "finished version header")
  (test-well-header)
  (println "finished well header")
  (test-curve-header)
  (println "finished curve header")
  (test-las-curves)
  (println "finished las curves")
  (test-las-file)
  (println "finished las file")
  (test-dollie)
  (println "finished dollie")
  (time (test-x4))
  (println "finished x4"))