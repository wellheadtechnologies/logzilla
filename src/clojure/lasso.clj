(ns lasso
  (:use util global)
  (:import (java.io PushbackReader BufferedReader InputStreamReader
		    BufferedWriter OutputStreamWriter StringReader
		    StringWriter)
	   (com.wellhead.lasso LasFileParser ClojureWriter ClojureReader)))

(defn replace-null-with-nan [data]
  (for [d data]
    (if (and (< d 0) 
	     (> 0.00001 (abs (- -999.25 d))))
      Double/NaN
      d)))

(defn load-lasfile [path]
  (let [reader (new LasFileParser)
	writer (new ClojureWriter)
	lf (.readLasFile reader path)
	lasfile (read-string (.writeLasFileToString writer lf))
	curves (:curves lasfile)
	index (first curves)
	sorted-index (assoc index :data (sort (:data index)))
	do-reverse (not= (first (:data index)) (first (:data sorted-index)))
	curves (rest curves)]
    (assoc lasfile 
      :path path
      :index index
      :curves 
      (for [curve curves]
	(assoc curve 
	  :index sorted-index
	  :data (replace-null-with-nan 
		 (if do-reverse
		   (reverse (:data curve))
		   (:data curve))))))))

(defn aggregate [indices srate]
  (let [datas (map :data indices)
	mins (map #(reduce min %) datas)
	maxes (map #(reduce max %) datas)
	_min (reduce min mins)
	_max (reduce max maxes)]
    {:data 
     (loop [i _min, acc []]
       (if (<= i _max)
	 (recur (+ i srate) (conj acc i))
	 acc))}))

(defn sample-rate [curve]
  (let [index (:index curve)
	index-data (:data index)]
    (abs (- (nth index-data 0) (nth index-data 1)))))

(defn adjust-curves [curves]
  (let [sample-rates (map sample-rate curves)]
    (println "sample-rates = " sample-rates)
    (guard (all-samef sample-rates)
	   "sample rates must all be the same")
    (let [indices (map :index curves)
	  srate (first sample-rates)
	  aggregate-index (aggregate indices srate)]
      [aggregate-index
       (let [_imin (round (first (:data aggregate-index)))
	     _imax (round (last (:data aggregate-index)))]
	   (println "_imin = " _imin)
	   (println "_imax = " _imax)
	   (for [curve curves]
	     (let [_cmin (round (reduce min (get-in curve [:index :data])))
		   _cmax (round (reduce max (get-in curve [:index :data])))
		   cdata (:data curve)
		   start-padding (/ (abs (- _cmin _imin)) srate)
		   end-padding (/ (abs (- _cmax _imax)) srate)]
	       (println "_cmin = " _cmin)
	       (println "_cmax = " _cmax)
	       (println "start-padding = " start-padding)
	       (println "end-padding = " end-padding)
	       (let [new-curve (assoc curve
				 :data (concat (repeat start-padding Double/NaN)
					       cdata
					       (repeat end-padding Double/NaN))
				 :index aggregate-index)
		     new-curve-size (count (:data new-curve))
		     aggregate-size (count (:data aggregate-index))]
		 (guard (= new-curve-size aggregate-size)
			(str "new-curve and aggregate index should be same size : " new-curve-size " vs " aggregate-size))
		 new-curve))))
       ])))
