(ns sources.contextmenu.controller
  (:use util gutil global file.contextmenu.view file.model)
  (:import (java.awt.event MouseEvent MouseAdapter)))

(defn edit [source-manager] 
  (open-curve-editor source-manager))

(defn _merge [source-manager]
  (open-curve-merger source-manager))

(defn copy [source-manager]
  (swing
   (let [file (get-selected-source source-manager)
	 curves (get-selected-curves (:curve-list @file))]
     (dosync (ref-set copied-curves curves)))))

(defn paste [source-manager]
  (swing
   (let [ccurves @copied-curves
	 file (get-selected-source source-manager)]
     (long-task 
      (doseq [curve ccurves]
	(add-curve file curve))))))

(defn _remove [source-manager] nil)

(defn init-listener [source-manager curve-list]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (create-context-menu
		     curve-list (.getX e) (.getY e) 
		     {:edit (partial edit source-manager)
		      :merge (partial _merge source-manager)
		      :copy (partial copy source-manager)
		      :paste (partial paste source-manager)
		      :remove (partial _remove source-manager)
		      })))))