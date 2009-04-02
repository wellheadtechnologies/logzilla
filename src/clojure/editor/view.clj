(ns editor.view
  (:use editor.model util gutil global)
  (:import (javax.swing JSlider JTable)
	   (javax.swing.table DefaultTableModel)))

(defn create-depth-slider [slider-notches]
  (let [slider (new JSlider 0 slider-notches 0)]
    (doto slider
      (.setOrientation JSlider/VERTICAL))))

(defn create-table [index curves]
  (let [model (new DefaultTableModel)
	table (new JTable model)]    
    (.addColumn model "x" (into-array Object (reverse (:data index))))
    (doseq [curve curves]
      (.addColumn model (get-in curve [:descriptor :mnemonic]) (into-array Object (reverse (:data curve)))))
    table))

(defn create-save-button [save-action]
  (button "Save" save-action))

(defn create-merge-button [merge-action]
  (button "Merge" merge-action))