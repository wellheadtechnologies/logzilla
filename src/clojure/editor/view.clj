(ns editor.view
  (:use editor.model util gutil global)
  (:import (javax.swing JSlider JTable)
	   (javax.swing.table DefaultTableModel)))

(defn create-depth-slider [editor-data]
  (let [slider (new JSlider 0 (:slider-notches editor-data) 0)]
    (doto slider
      (.setOrientation JSlider/VERTICAL))))

(defn create-table [editor-data]
  (let [{:keys [curves index]} editor-data
	model (new DefaultTableModel)
	table (new JTable model)
	index-data (large-to-small (:data index))
	do-reverse (not= (first index-data) (first (:data index)))]    
    (.addColumn model (get-in index [:descriptor :mnemonic]) (into-array Object index-data))
    (doseq [curve curves]
      (.addColumn model (get-in curve [:descriptor :mnemonic])
		  (if do-reverse 
		    (into-array Object (reverse (:data curve)))
		    (into-array Object (:data curve)))))
    table))

(defn create-save-button [editor-data]
  (button "Save" (fn [e] nil)))

(defn create-merge-button [editor-data]
  (button "Merge" (fn [e] nil)))