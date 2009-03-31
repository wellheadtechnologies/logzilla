(ns lasso
  (:use util global)
  (:import (java.io PushbackReader BufferedReader InputStreamReader
		    BufferedWriter OutputStreamWriter StringReader
		    StringWriter)
	   (com.wellhead.lasso LasFileParser ClojureWriter ClojureReader CurveUtil)))

(def pad-curve)

(defn load-lasfile [path]
  (let [reader (new LasFileParser)
	writer (new ClojureWriter)
	lf (.readLasFile reader path)]
    (doseq [curve (.getCurves lf)]
      (.replaceCurve lf curve (CurveUtil/adjustCurve (.getIndex lf) curve)))
    (let [lasfile (read-string (.writeLasFileToString writer lf))
	  curves (:curves lasfile)
	  index (first curves)
	  curves (rest curves)
	  indexed-curves (map #(assoc % :index index) curves)
	  evaled-curves (for [c indexed-curves] (assoc c :data (map eval (:data c))))]
      (assoc lasfile 
	:path path
	:index index
	:curves evaled-curves))))

(defn pad-curve [index curve]
  (let [reader (new ClojureReader)
	writer (new ClojureWriter)
	whindex (.parseCurve reader index)
	whcurve (.parseCurve reader curve)
	adjusted (CurveUtil/adjustCurve whindex (doto whcurve (.setIndex whindex)))
	curve (read-string (.writeCurveToString writer adjusted))
	index-curve (assoc curve :index index)
	evaled-curve (assoc index-curve :data (map eval (:data index-curve)))]
    evaled-curve))

