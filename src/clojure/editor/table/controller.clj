(ns editor.table.controller
  (:use util gutil storage global
	editor.table.view)
  (:import (javax.swing.event TableModelListener)))

(defn show-cell [table row col]
  (swing-io!
   (let [rect (.getCellRect table row col true)]
     (.scrollRectToVisible table rect))))

(defn show-percentage [table n]
  (let [n (abs (- 1 n))]
    (swing-io!   
      (guard (not (or (> n 1) (< n 0)))
	     (str "invalid n must be from 0.0 to 1.0: " n))
      (let [rows (dec (.getRowCount table))
	    row (* n rows)]
	(show-cell table row 0)))))

(defn push-to-curve [table curve-id] nil)

(defn pull-from-curve [table curve-id] nil)

(defn init-listener [editor-id curve-id table]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (when (invoke [editor-id :not-dragging-anything])
		    (push-to-curve table curve-id)))))

(defn init-table [editor-id curve-ids aggregate-index dirty-curves]
  (let [table (create-table aggregate-index dirty-curves)
	model (.getModel table)]
    (doseq [curve-id curve-ids]
      (.addTableModelListener model (init-listener editor-id curve-id table)))
    table))

