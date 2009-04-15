(ns file.contextmenu.view
  (:use gutil util global)
  (:import (javax.swing JPopupMenu JMenuItem)))

(defn create-context-menu [curve-list x y cm-actions]
  (let [m (JPopupMenu.)
	edit (JMenuItem. "Edit")
	merge (JMenuItem. "Merge")
	copy (JMenuItem. "Copy")
	paste (JMenuItem. "Paste")
	remove (JMenuItem. "Remove")]

    (swing
     (set-action edit (:edit cm-actions))
     (set-action merge (:merge cm-actions))
     (set-action copy (:copy cm-actions))
     (set-action paste (:paste cm-actions))
     (set-action remove (:remove cm-actions))
     
     (let [svc (count (.getSelectedValues curve-list))]
       (cond 
	(= 0 svc)
	(do 
	  (.setEnabled edit false)
	  (.setEnabled merge false))
	
	(= 1 svc)
	(do 
	  (.setEnabled edit true)
	  (.setEnabled merge false))
	
	(< 1 svc)
	(do 
	  (.setEnabled edit false)
	  (.setEnabled merge true))))

     (doto m
       (.add edit)
       (.add merge)
       (.add copy)
       (.add paste)
       (.add remove)
       (.show curve-list x y)))))

