(ns editor.table.controller
  (:use util gutil global
	editor.table.view editor.table.model)
  (:import (javax.swing.event TableModelListener)
	   (javax.swing JScrollPane)))

(defn show-cell [widget row col]
  (swing-io!
   (let [rect (.getCellRect widget row col true)]
     (.scrollRectToVisible widget rect))))

(defn show-percentage [widget n]
  (let [n (abs (- 1 n))]
    (swing-io!   
      (guard (not (or (> n 1) (< n 0)))
	     (str "invalid n must be from 0.0 to 1.0: " n))
      (let [rows (dec (.getRowCount widget))
	    row (* n rows)]
	(show-cell widget row 0)))))

(defn init-listener [table]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (let [row (.getFirstRow e)
			col (.getColumn e)]
		    (dosync 
		     (alter table assoc :altered [row col]))))))

(defn init-table [aggregate-index dirty-curves]
  (let [widget (create-table-widget aggregate-index dirty-curves)
	pane (JScrollPane. widget)
	model (.getModel widget)
	table (ref (struct-map Table
		     :pane pane
		     :widget widget))]
    (.addTableModelListener model (init-listener table))
    table))

