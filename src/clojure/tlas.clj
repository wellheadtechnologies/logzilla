(ns tlas
  (:use util))

(import '(core LasFile DefaultLasFile
	       Curve DefaultCurve
	       Header DefaultHeader
	       Headers
	       Descriptor DefaultDescriptor)
	'(java.util List))
	

(defn find-descriptor [descriptors name]
  (find-first #(= name (.getMnemonic %)) descriptors))

(defn create-curve [descriptors name index data] 
  (let [descriptor (find-descriptor descriptors name)
	ldata (to-linked-list data)]
    (new DefaultCurve descriptor index ldata)))

(def well-descriptors
     [(new DefaultDescriptor "STRT" "m" 1499.8790000 nil)
      (new DefaultDescriptor "STOP" "m" 2416.3790000 nil)
      (new DefaultDescriptor "STEP" "m" 0.00000000 nil)
      (new DefaultDescriptor "NULL" nil -999.250000 nil)
      (new DefaultDescriptor "COMP" nil nil "COMPANY")
      (new DefaultDescriptor "WELL" nil "A10" "WELL")
      (new DefaultDescriptor "FLD" nil nil "FIELD")
      (new DefaultDescriptor "LOC" nil nil "LOCATION")
      (new DefaultDescriptor "SRVC" nil nil "SERVICE COMPANY")
      (new DefaultDescriptor "DATE" nil "Monday, January 26 2009 14:04:02" "DATE")
      (new DefaultDescriptor "PROV" nil nil "PROVINCE")
      (new DefaultDescriptor "UWI" nil nil "UNIQUE WELL ID")
      (new DefaultDescriptor "API" nil nil "API NUMBER")])

(def curve-descriptors 
     [(new DefaultDescriptor "DEPT" "m" nil "DEPTH")
      (new DefaultDescriptor "NetGross" nil nil "NetGross")
      (new DefaultDescriptor "Facies" nil nil "Facies")
      (new DefaultDescriptor "Porosity" "m3/m3" nil "Porosity")
      (new DefaultDescriptor "Gamma" "gAPI" nil "Gamma")
      (new DefaultDescriptor "DEPTH" "m" nil "trend")])

(def parameter-descriptors [])

(def headers
     [(Headers/VersionHeader "2.0" "NO")
      (Headers/WellHeader well-descriptors)
      (Headers/CurveHeader curve-descriptors)
      (Headers/ParameterHeader parameter-descriptors)])

(def curves 
     (let [new-curve (partial create-curve curve-descriptors)
	   dept (new-curve "DEPT" nil (take 9 (iterate #(+ 0.5 %) 1499.879)))
	   netgross (new-curve "NetGross" dept (take 9 (repeat 0)))
	   facies (new-curve "Facies" dept
			     (concat (take 3 (repeat -999.25)) 
				     (take 6 (repeat 0))))
	   porosity (new-curve "Porosity" dept
			       (concat (take 3 (repeat -999.25))
				       [0.2706460059
					0.2674280107
					0.2560760081
					0.2421260029
					0.2385890037
					0.2383770049]))
	   gamma (new-curve "Gamma" dept
			    (concat (take 4 (repeat -999.25))
				    [78.869453430
				     78.008300781
				     75.581558228
				     73.238037109
				     71.504173279]))
	   depth (new-curve "DEPTH" dept
			    (take 9 (iterate #(+ 0.5 %) 1499.8790283)))]
       [dept netgross facies porosity gamma depth]))

(defn test-instantiation []
  (let [lf1 (new DefaultLasFile headers curves)
	lf2 (new DefaultLasFile headers curves)]
    (assert (= curves (.getCurves lf1)))
    (assert (= headers (.getHeaders lf1)))
    (assert (= lf1 lf2))))

(defn run-tests []
  (test-instantiation)
  (println "finished testing instantiation"))