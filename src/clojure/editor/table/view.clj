(ns editor.table.view
  (:import (javax.swing JTable JScrollPane)
	   (javax.swing.table DefaultTableModel)))

(defn create-table [index curves]
  (let [model (DefaultTableModel.)
	table (JTable. model)]    
    (.addColumn model "x" (into-array Object (reverse (:data index))))
    (doseq [curve curves]
      (.addColumn model 
		  (get-in curve [:descriptor :mnemonic])
		  (into-array Object (reverse (:data curve)))))
    table))

(defn create-table-pane [table] (JScrollPane. table))
