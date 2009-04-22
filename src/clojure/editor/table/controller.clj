(ns editor.table.controller
  (:use util gutil global
	editor.table.view editor.table.model
	messages)
  (:import (javax.swing.event TableModelListener)
	   (javax.swing JScrollPane JOptionPane)))

(defn show-cell [widget row col]
  (swing-io!
   (let [rect (.getCellRect widget row col true)]
     (.scrollRectToVisible widget rect))))

(defn show-percentage [table percentage]
  (let [widget (:widget @table)
	n percentage]
    (dosync 
     (when (not= (:percentage @table) percentage)
       (alter table assoc :percentage percentage)
       (swing
	(when (not (or (> n 1) (< n 0)))
	  (let [rows (dec (.getRowCount widget))
		row (* n rows)]
	    (show-cell widget row 0))))))))

(defn init-listener [table]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (let [row (.getFirstRow e)
			col (.getColumn e)
			val (.. (:widget @table) (getModel) (getValueAt row col))]
		    (try (Double/valueOf val) 
			 (catch NumberFormatException e 
			   (JOptionPane/showMessageDialog 
			    (:widget @table) (str val " doesn't appear to be properly formatted")
			    "Number Format Error" JOptionPane/ERROR_MESSAGE)))
		    (fire :value-change table {:row row
					       :col col
					       :value val})))))

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