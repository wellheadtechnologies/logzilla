(ns editor.model
  (:load "/lasso"))

(defn largest-index [curves]
  (:index 
   (reduce
    (fn [x y]
      (if (> (count (get-in x [:index :data]))
	     (count (get-in y [:index :data])))
	x y))
    curves)))


(defn index-to-row [index table]
  (- (dec (.getRowCount table)) index))

(defn row-to-index [row table]
  (- (dec (.getRowCount table)) row))

(defn get-scale [editor-data]
  (let [{:keys [max-depth min-depth scale-notches]} editor-data
	diff (- max-depth min-depth)
	scale (/ diff scale-notches)]
    scale))

(defn scale-value [editor-data value]
  (let [scale (get-scale editor-data)
	{:keys [slider-notches scale-notches]} editor-data
	ratio (/ slider-notches scale-notches)]
    (* (/ value ratio) scale)))
