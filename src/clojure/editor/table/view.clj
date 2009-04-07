(ns editor.table.view
  (:import (javax.swing JTable JScrollPane)
	   (javax.swing.table DefaultTableModel)))

(defn create-table-widget [index curves]
  (let [model (DefaultTableModel.)
	widget (JTable. model)]    
    (.addColumn model "x" (into-array Object (reverse (:data index))))
    (doseq [curve curves]
      (.addColumn model 
		  (get-in curve [:descriptor :mnemonic])
		  (into-array Object (reverse (:data curve)))))
    widget))

(defn create-table-pane [table] (JScrollPane. table))
