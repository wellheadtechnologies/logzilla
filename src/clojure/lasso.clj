(ns lasso
  (:use util)
  (:import (java.io PushbackReader BufferedReader InputStreamReader
		    BufferedWriter OutputStreamWriter)))

;(defstruct LasFile :name :path :headers :curves)
;(defstruct Header :type :prefix :descriptors)
;(defstruct Descriptor :mnemonic :unit :data :description)
;(defstruct Curve :descriptor :data :index)

(defn proc-writer [proc]
  (new BufferedWriter (new OutputStreamWriter (.getOutputStream proc))))

(defn proc-reader [proc]
  (new PushbackReader (new BufferedReader (new InputStreamReader (.getInputStream proc)))))

(defn with-proc-reader [proc fun]
  (let [reader (proc-reader proc)]
    (try 
     (fun reader)
     (finally 
      (.close reader)))))

(defn with-proc-writer [proc fun]
  (let [writer (proc-writer proc)]
    (try 
     (fun writer)
     (finally 
      (.close writer)))))

(defn load-lasfile [path]
  (let [proc (exec (str "./lasso " path " clojure://stdout"))]
    (with-proc-reader proc
      (fn [reader]
	(let [lasfile (read reader)
	      curves (:curves lasfile)
	      index (first curves)]
	  (assoc lasfile :path path :index index
		 :curves (map #(assoc % :index index) curves)))))))

(defn pad-curve [index curve]
  (let [proc (exec (str "./lasso pad --stream clojure"))]
    (with-proc-writer proc 
      (fn [writer]
	(doto writer
	  (.write index)
	  (.newLine)
	  (.write curve)
	  (.close))))
    (with-proc-reader proc 
      (fn [reader]
	(let [padded-curve (read reader)]
	  (assoc padded-curve :index index))))))
