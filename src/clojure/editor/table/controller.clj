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

(defn init-listener [table-id]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (let [row (.getFirstRow e)
			col (.getColumn e)]
		    (dosync 
		     (change-in table-id [:altered] [row col]))))))

(defn get-instance-properties [table]
  (instance-properties
   [:altered []]))

(defn init-table [aggregate-index dirty-curves]
  (let [table (create-table aggregate-index dirty-curves)
	table-id table
	model (.getModel table)
	props (get-instance-properties table)]
    (store-properties table props)
    (.addTableModelListener model (init-listener table-id))
    table-id))

