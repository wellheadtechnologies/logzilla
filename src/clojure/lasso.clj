(ns lasso
  (:use util global)
  (:import (java.io PushbackReader BufferedReader InputStreamReader
		    BufferedWriter OutputStreamWriter StringReader
		    StringWriter)
	   (com.wellhead.lasso LasFileParser ClojureWriter ClojureReader LasFileWriter)))

(defn replace-null-with-nan [data]
  (for [d data]
    (if (and (< d 0) 
	     (> 0.00001 (abs (- -999.25 d))))
      Double/NaN
      d)))

(defn replace-nan-with-null [data]
  (for [d data]
    (if (.isNaN d)
      -999.25
      d)))

(defn remove-nulls [lasfile]
  (let [curves (:curves lasfile)]
    (assoc lasfile
      :curves 
      (for [curve curves]
	(assoc curve
	  :data (replace-null-with-nan (:data curve)))))))

(defn insert-nulls [lasfile]
  (let [curves (:curves lasfile)]
    (assoc lasfile 
      :curves 
      (for [curve curves]
	(assoc curve 
	  :data (replace-nan-with-null (:data curve)))))))

(defn deref-curve [curve]
  (assoc curve :index @(:index curve)))

(defn- referrize-lasfile [lasfile]
  (let [index (:index lasfile)
	curves (:curves lasfile)
	headers (:headers lasfile)
	referrized-index (ref index)
	curves (map #(assoc % :index referrized-index) curves)
	referrized-curves (doall (map ref curves))
	referrized-headers (doall (map ref headers))
	referrized-lasfile (ref 
			    (assoc lasfile 
			      :index referrized-index
			      :curves referrized-curves
			      :headers referrized-headers))]
    referrized-lasfile))

(defn load-lasfile [path]
  (referrize-lasfile 
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
       :index sorted-index
       :do-reverse do-reverse
       :curves 
       (for [curve curves]
	 (assoc curve 
	   :index sorted-index
	   :data (apply vector (replace-null-with-nan 
				(if do-reverse
				  (reverse (:data curve))
				  (:data curve))))))))))

(defn save-lasfile [lasfile]
  (let [lasfile (assoc lasfile :curves (concat [(:index lasfile)] (:curves lasfile)))
	lasfile (assoc lasfile :curves (map #(dissoc % :index) (:curves lasfile)))
	lasfile (insert-nulls lasfile)
	lasfile (assoc lasfile :curves 
		       (if (:do-reverse lasfile)
			 (for [curve (:curves lasfile)]
			   (assoc curve :data (reverse (:data curve))))
			 (:curves lasfile)))
	lasfile (dissoc lasfile :index)]
    (let [reader (new ClojureReader)
	  writer (new LasFileWriter)
	  whlf (.parseLasFile reader lasfile)]
      (.writeLasFile writer whlf (str (:path lasfile) ".saved")))))

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
  (let [curves (doall (map deref-curve curves))]
    (let [sample-rates (map sample-rate curves)]
      (guard (all-samef sample-rates)
	     "sample rates must all be the same")
      (let [indices (map :index curves)
	    srate (first sample-rates)
	    aggregate-index (aggregate indices srate)]
	[aggregate-index
	 (let [_imin (round (first (:data aggregate-index)))
	       _imax (round (last (:data aggregate-index)))]
	   (for [curve curves]
	     (let [_cmin (round (reduce min (get-in curve [:index :data])))
		   _cmax (round (reduce max (get-in curve [:index :data])))
		   cdata (:data curve)
		   start-padding (/ (abs (- _cmin _imin)) srate)
		   end-padding (/ (abs (- _cmax _imax)) srate)]
	       (let [new-curve (assoc curve
				 :data (apply vector (concat (repeat start-padding Double/NaN)
							     cdata
							     (repeat end-padding Double/NaN)))
				 :index aggregate-index)
		     new-curve-size (count (:data new-curve))
		     aggregate-size (count (:data aggregate-index))]
		 (guard (= new-curve-size aggregate-size)
			(str "new-curve and aggregate index should be same size : " new-curve-size " vs " aggregate-size))
		 new-curve))))
	 ]))))
