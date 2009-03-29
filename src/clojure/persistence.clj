(ns persistence 
  (:use util)
  (:import (java.io PushbackReader BufferedReader InputStreamReader)))

;(defstruct LasFile :name :path :headers :curves)
;(defstruct Header :type :prefix :descriptors)
;(defstruct Descriptor :mnemonic :unit :data :description)
;(defstruct Curve :descriptor :data :index)

(defn load-lasfile [path]
  (let [proc (exec (str "./lasso " path " clojure://stdout"))]
    (let [reader (new PushbackReader (new BufferedReader (new InputStreamReader (.getInputStream proc))))
	  lasfile (read reader)
	  curves (:curves lasfile)
	  index (first curves)]
      (assoc lasfile :path path :index index
	     :curves (map #(assoc % :index index) curves)))))