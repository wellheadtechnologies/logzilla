(ns sources.controller)

(defn- create-source-tree []
  (tree 
   ["" 
    "Las Files" 
    "Other"]))

(defn- custom-tree-payload [file]
  (proxy [NodePayload] []
    (toString [] 
	      (dosync 
	       (let [lasfile (:lasfile @file)]
		 (:name @lasfile))))
    (getFile [] file)))

(defn- init-source-tree-selection-listener [source-manager]
  (proxy [TreeSelectionListener] []
    (valueChanged [e]
		  (let [path (.getNewLeadSelectionPath e)]
		    (when path
		      (let [leaf (.getLastPathComponent path)
			    payload (.getUserObject leaf)
			    file (.getFile payload)]
			(set-selected-source source-manager file)
			(update-log-tab (:lasfile @file))))))))


(defn- init-source-tree [source-manager]
  (let [source-tree (create-source-tree)
	renderer (.getCellRenderer source-tree)]
    (doto source-tree
      (.addTreeSelectionListener (init-source-tree-selection-listener source-manager)))))
