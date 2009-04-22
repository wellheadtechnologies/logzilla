(ns editor.table.view
  (:use gutil)
  (:import (javax.swing JTable JScrollPane DefaultListSelectionModel)
	   (javax.swing.table DefaultTableModel DefaultTableCellRenderer TableCellRenderer)
	   (java.awt Color)
	   (org.jdesktop.swingx.decorator HighlighterFactory)
	   (org.jdesktop.swingx JXTable)))

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
