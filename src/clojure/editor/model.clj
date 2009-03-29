(ns editor.model
  (:load "/lasso"))

(defn largest-index [curves]
  (reduce
   (fn [x y]
     (if (> (count (get-in x [:index :data]))
	    (count (get-in y [:index :data])))
       x y))
   curves))

(defn prepare-curves [index curves]  
  (for [curve curves]
    (pad-curve index curve)))


