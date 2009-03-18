(ns tparser  
  (:use util data))

(import '(java.io File)
	'(core DefaultLasParser DefaultLasWriter))

(defn test-las-file []
  (let [lf (DefaultLasParser/parseLasFile "las_files/test.las")
	dept (.getIndex lf)
	gamma (.getCurve lf "Gamma")
	porosity (.getCurve lf "Porosity")]
    (assert (= 1501.629 (nth (.getLasData dept) 0)))
    (assert (= "gAPI" (.getUnit gamma)))
    (assert (= "m3/m3" (.getUnit porosity)))
    (assert (= "DEPTH" (.getDescription dept)))

    (let [hs (.getHeaders lf)
	  not-nil #(assert (not (nil? %)))]
      (assert (= 4 (count hs)))
      (not-nil (.getVersionHeader lf))
      (not-nil (.getCurveHeader lf))
      (not-nil (.getWellHeader lf))
      (not-nil (.getParameterHeader lf)))))

(defn test-dollie []
  (let [lf (DefaultLasParser/parseLasFile "las_files/dollie.las")
	dept (.getIndex lf)
	wtoc (.getCurve lf "WTOC")]
    (assert (not (nil? dept)))
    (assert (not (nil? wtoc)))
    (assert (= 7800 (nth (.getLasData dept) 0)))
    (assert (= 6680 (last (.getLasData dept))))
    (assert (= "LBF/LBF" (.getUnit wtoc)))))

(defn test-x4 []
  (let [lf (DefaultLasParser/parseLasFile "las_files/x4.las")
	wh (.getWellHeader lf)
	strt (.getData (.getDescriptor wh "STRT"))
	stop (.getData (.getDescriptor wh "STOP"))]
    (assert (not (nil? wh)))
    (assert (not (nil? strt)))
    (assert (not (nil? stop)))
    (assert (= strt "57.000000000"))
    (assert (= stop "5817.0000000"))))

(defn test-parse-all []
  (let [directory (new File "las_files")
	files (.listFiles directory)]
    (doseq [file files]
      (DefaultLasParser/parseLasFile file))))

(defn test-write-lasfile []
  (let [in-out 
	(fn [path] 
	  (let [lf1 (DefaultLasParser/parseLasFile path)]
	    (DefaultLasWriter/writeLasFile lf1 "output_test.las")
	    (let [lf2 (DefaultLasParser/parseLasFile "output_test.las")]
	      (= lf1 lf2))))]
  
    (time (in-out "las_files/test.las"))
    (time (in-out "las_files/dollie.las"))
    (time (in-out "las_files/robert.las"))
    (time (in-out "las_files/x4.las"))))


(defn run-tests []
  (test-las-file)
  (println "finished las file")

  (test-write-lasfile)
  (println "finished writing lasfile")

  (test-dollie)
  (println "finished dollie")
  (time (test-x4))
  (println "finished x4")
  
  (test-parse-all)
  (println "finished test parse all"))