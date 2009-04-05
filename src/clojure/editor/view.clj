(ns editor.view
  (:use editor.model util gutil global)
  (:import (javax.swing JSlider JTable)
	   (javax.swing.table DefaultTableModel)))

(defn create-save-button [save-action]
  (button "Save" save-action))

(defn create-merge-button [merge-action]
  (button "Merge" merge-action))