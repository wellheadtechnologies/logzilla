(ns editor.table.controller
  (:use util gutil global
	editor.table.view editor.table.model)
  (:import (javax.swing.event TableModelListener)
	   (javax.swing JScrollPane)))

(declare fire-value-change-event)

(defn show-cell [widget row col]
  (swing-io!
   (let [rect (.getCellRect widget row col true)]
     (.scrollRectToVisible widget rect))))

(defn show-percentage [table percentage]
  (let [widget (:widget @table)
	n (abs (- 1 percentage))]
    (dosync 
     (when (not= (:percentage @table) percentage)
       (alter table assoc :percentage percentage)
       (swing
	 (guard (not (or (> n 1) (< n 0)))
		(str "invalid n must be from 0.0 to 1.0: " n))
	 (let [rows (dec (.getRowCount widget))
	       row (* n rows)]
	   (show-cell widget row 0)))))))

(defn init-listener [table]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (let [row (.getFirstRow e)
			col (.getColumn e)
			val (.. (:widget @table) (getModel) (getValueAt row col))]
		    (fire-value-change-event table row col val)))))

(defn init-table [index dirty-curve]
  (let [widget (create-table-widget index [dirty-curve])
	pane (JScrollPane. widget)
	model (.getModel widget)
	table (ref (struct-map Table
		     :pane pane
		     :widget widget
		     :value-change-listeners []))]
    (.addTableModelListener model (init-listener table))
    table))


(defn fire-value-change-event [table row col value]
  (let [event {:row row :col col :value value}
	listeners (:value-change-listeners @table)]
    (fire-event listeners event)))

(defn add-value-change-listener [table listener]
  (add-listener :value-change-listeners table listener))

(defn remove-value-change-listener [table listener]
  (remove-listener :value-change-listeners table listener))