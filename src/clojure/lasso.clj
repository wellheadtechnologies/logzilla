(ns lasso
  (:use util global)
  (:import (java.io PushbackReader BufferedReader InputStreamReader
		    BufferedWriter OutputStreamWriter StringReader
		    StringWriter)
	   (com.wellhead.lasso LasFileParser ClojureWriter ClojureReader CurveUtil)))
  
;(let [proc (exec (str "./lasso " path " clojure://stdout"))]
;(with-proc-reader proc
;      (fn [reader]
;	(let [lasfile (read reader)
;	      curves (:curves lasfile)
;	      index (first curves)]
;	  (assoc lasfile :path path :index index
;		 :curves (map #(assoc % :index index) curves)))))))
;
;(defn pad-curve [index curve]
;  (let [proc (exec (str "./lasso pad --stream clojure"))]
;    (long-task 
;     (with-proc-writer proc 
;       (fn [writer]
;	 (doto writer
;	   (.write (str index))
;	   (.newLine)
;	   (.write (str curve))
;	   (.close)))))
;    (with-proc-reader proc 
;      (fn [reader]
;	(let [padded-curve (read reader)]
;	  (assoc padded-curve :index index))))))


(defn load-lasfile [path]
  (let [reader (new LasFileParser)
	writer (new ClojureWriter)
	lf (.readLasFile reader path)
	lasfile	(read (new PushbackReader 
			   (new BufferedReader 
				(new StringReader
				     (.writeLasFileToString writer lf)))))
	curves (:curves lasfile)
	index (first curves)]
    (assoc lasfile :path path :index index
	   :curves (map #(assoc % :index index) curves))))

(defn pad-curve [index curve]
  (let [reader (new ClojureReader)
	writer (new ClojureWriter)
	whindex (.parseCurve reader index)
	whcurve (.parseCurve reader curve)
	adjusted (CurveUtil/adjustCurve whindex (doto whcurve (.setIndex whindex)))
	curve (read (new PushbackReader
			 (new BufferedReader
			      (new StringReader
				   (.writeCurveToString writer adjusted)))))]
    (assoc curve :index index)))