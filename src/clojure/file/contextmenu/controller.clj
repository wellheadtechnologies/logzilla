(ns file.contextmenu.controller
  (:use util gutil global file.contextmenu.view file.model)
  (:import (java.awt.event MouseEvent MouseAdapter)))

(defn edit [e] nil)

(def get-selected-curves nil)

(defn copy [file-manager]
  (fn [e]
    (swing
     (let [file (fm-invoke :get-selected-file file-manager)
	   curves (fm-invoke :get-selected-curves (:curve-list @file))]
       (dosync (ref-set copied-curves curves))))))

(defn paste [file-manager]
  (fn [e]
    (swing
     (let [ccurves @copied-curves
	   file (fm-invoke :get-selected-file file-manager)]
       (long-task 
	(doseq [curve ccurves]
	  (fm-invoke :add-curve file curve)))))))

(defn _remove [e] nil)

(defn init-listener [file-manager curve-list]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (create-context-menu
		     curve-list (.getX e) (.getY e) 
		     {:edit edit
		      :copy (copy file-manager)
		      :paste (paste file-manager)
		      :remove _remove
		      })))))