(ns sources.controller)

(declare open-curve-editor get-selected-source
	 get-selected-curves add-curve 
	 open-curve-merger
	 create-context-menu)

(defn context-menu-edit [source-manager] 
  (open-curve-editor source-manager))

(defn context-menu-merge [source-manager]
  (open-curve-merger source-manager))

(defn context-menu-copy [source-manager]
  (swing
   (let [file (get-selected-source source-manager)
	 curves (get-selected-curves (:curve-list @file))]
     (dosync (ref-set copied-curves curves)))))

(defn context-menu-paste [source-manager]
  (swing
   (let [ccurves @copied-curves
	 file (get-selected-source source-manager)]
     (long-task 
      (doseq [curve ccurves]
	(add-curve file curve))))))

(defn context-menu-remove [source-manager] nil)

(defn init-context-menu-listener [source-manager curve-list]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (create-context-menu
		     curve-list (.getX e) (.getY e) 
		     {:edit (partial context-menu-edit source-manager)
		      :merge (partial context-menu-merge source-manager)
		      :copy (partial context-menu-copy source-manager)
		      :paste (partial context-menu-paste source-manager)
		      :remove (partial context-menu-remove source-manager)
		      })))))
