(ns lasfile.contextmenu.controller
  (:require editor.controller) 
  (:use util gutil global lasfile.contextmenu.view lasfile.model)
  (:import (java.awt.event MouseEvent MouseAdapter)))

(defstruct ContextMenuConfig
  :curve-list :x :y
  :edit-action
  :copy-action
  :paste-action
  :remove-action)

(defn edit []
  (swing-sync
   (let [selected-curves (get-selected-curves)]
     (when (< 0 (count selected-curves))
       (long-task 
	 (editor.controller/open-curve-editor
	  @selected-lasfile selected-curves))))))

(defn copy []
  (swing-sync
   (let [selected-curves (get-selected-curves)]
     (ref-set copied-curves selected-curves))))

(defn paste [add-curve]
  (swing-sync 
   (let [ccurves @copied-curves]
     (let [old-lasfile @selected-lasfile
	   curve-list (get @curve-lists old-lasfile)
	   old-curves (:curves old-lasfile)
	   new-curves (concat old-curves ccurves)
	   new-lasfile (assoc old-lasfile :curves new-curves)]
       (long-task 
	 (doseq [curve ccurves]
	   (add-curve curve-list curve)))
       (ref-set lasfile-list (replace {old-lasfile new-lasfile} @lasfile-list))))))

(defn default-edit-action [e] (edit))
(defn default-copy-action [e] (copy))
(defn default-paste-action [add-curve] (fn [e] (paste add-curve)))
(defn default-remove-action [e] nil)

(defn init-listener [config]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (create-context-menu 
		     (assoc config :x (.getX e) :y (.getY e)))))))

(defn init-default-listener [curve-list add-curve]
  (init-listener 
   (struct-map ContextMenuConfig
     :curve-list curve-list
     :edit-action default-edit-action
     :copy-action default-copy-action
     :paste-action (default-paste-action add-curve)
     :remove-action default-remove-action)))