(ns sources.controller)

(declare open-curve-editor get-selected-source
	 get-selected-curves add-curve 
	 open-curve-merger
	 create-context-menu)

(defn- context-menu-edit [source-manager] 
  (open-curve-editor source-manager))

(defn- context-menu-merge [source-manager]
  (open-curve-merger source-manager))

(defn- context-menu-copy [source-manager]
  (swing-agent
   (let [file (get-selected-source source-manager)
	 curves (get-selected-curves (:curve-list @file))]
     (dosync (ref-set copied-curves curves)))))

(defn- context-menu-paste [source-manager]
  (swing-agent
   (let [ccurves @copied-curves
	 file (get-selected-source source-manager)]
     (long-task 
      (doseq [curve ccurves]
	(add-curve file curve))))))

(defn- context-menu-remove [source-manager] nil)
(defn- create-context-menu [source-manager curve-list x y]
  (let [m (JPopupMenu.)
	edit (JMenuItem. "Edit")
	merge (JMenuItem. "Merge")
	copy (JMenuItem. "Copy")
	paste (JMenuItem. "Paste")
	remove (JMenuItem. "Remove")]

    (swing-agent
     (set-action edit #(context-menu-edit source-manager))
     (set-action merge #(context-menu-merge source-manager))
     (set-action copy #(context-menu-copy source-manager))
     (set-action paste #(context-menu-paste source-manager))
     (set-action remove #(context-menu-remove source-manager))
     
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

(defn- init-context-menu-listener [source-manager curve-list]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (create-context-menu source-manager curve-list (.getX e) (.getY e))))))
