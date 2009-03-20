(ns core.las
  (:use util))

(defn get-curve [name curves]
  (find-first #(= (.getMnemonic %) name) curves))

(defn largest-index [curves]
  (.getIndex
   (reduce 
    (fn [c1 c2]
      (let [i1 (.getIndex c1)
	    i2 (.getIndex c2)]
	(if (> (count (.getLasData i1)) (count (.getLasData i2)))
	  c1
	  c2)))
    curves)))

(defn large-to-small [coll]
  (reverse (sort coll)))

(defn get-offsets [pindex-data curves]
  (let [index-datas (map #(large-to-small (.getLasData (.getIndex %)))
		     curves)
	start (first pindex-data)
	finish (last pindex-data)]
    (for [index-data index-datas]
      (index-of start index-data))))


      