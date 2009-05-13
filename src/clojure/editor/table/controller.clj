(ns editor.table.controller
  (:use util gutil global messages)
  (:import (javax.swing.event TableModelListener)
	   (javax.swing JScrollPane JOptionPane)
	   (javax.swing JTable JScrollPane DefaultListSelectionModel)
	   (javax.swing.table DefaultTableModel DefaultTableCellRenderer TableCellRenderer)
	   (java.awt Color)
	   (org.jdesktop.swingx.decorator HighlighterFactory)
	   (org.jdesktop.swingx JXTable)))

(defstruct Table 
  :pane
  :widget
  :percentage
  :value-change-listeners)

(defn custom-table-model []
  (proxy [DefaultTableModel] []
    (isCellEditable [r c] (not= c 0))))

(defn create-table-widget [index curves]
  (let [model (custom-table-model)
	widget (JXTable. model)]    
    (doto widget
      (.setSelectionModel (single-selection-model))
      (.addHighlighter (HighlighterFactory/createSimpleStriping)))
    (.addColumn model "x" (into-array Object (:data index)))
    (doseq [curve curves]
      (.addColumn model 
		  (get-in curve [:descriptor :mnemonic])
		  (into-array Object (:data curve))))
    widget))

(defn create-table-pane [table] (JScrollPane. table))

(defn show-cell [widget row col]
  (swing-mutator
   (let [rect (.getCellRect widget row col true)]
     (.scrollRectToVisible widget rect))))

(defmulti show-percentage (fn [x y]
			    (cond 
			     (number? y) :percentage
			     (= (class y) clojure.lang.PersistentArrayMap) :event
			     )))

(defmethod show-percentage :event [table event]
  (show-percentage table (:percentage event)))

(defmethod show-percentage :percentage [table percentage]
  (let [widget (:widget @table)
	n percentage]
    (dosync 
     (when (not= (:percentage @table) percentage)
       (alter table assoc :percentage percentage)
       (swing-agent
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
    (add-receiver :percentage-change table #(show-percentage table %))
    table))
