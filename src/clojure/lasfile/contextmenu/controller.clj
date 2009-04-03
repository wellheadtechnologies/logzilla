(ns lasfile.contextmenu.controller
  (:require editor.controller) 
  (:use util gutil global lasfile.contextmenu.view lasfile.model storage)
  (:import (java.awt.event MouseEvent MouseAdapter)))

(defstruct ContextMenuConfig
  :curve-list :x :y
  :edit-action
  :copy-action
  :paste-action
  :remove-action)

(defn edit []
  (invoke :open-curve-editor))

(defn copy []
  (swing-sync
   (let [selected-curve-ids (get-selected-curve-ids)]
     (ref-set copied-curve-ids selected-curve-ids))))

(defn paste [add-curve]
  (swing-sync 
   (let [ccurves @copied-curve-ids]
     (let [lasfile-id @selected-lasfile-id
	   old-lasfile (lookup lasfile-id)
	   old-curves (:curves old-lasfile)
	   new-curves (concat old-curves ccurves)
	   new-lasfile (assoc old-lasfile :curves new-curves)
	   curve-list (get @curve-lists lasfile-id)]
       (update lasfile-id new-lasfile)
       (long-task 
	 (doseq [curve ccurves]
	   (add-curve curve-list curve)))))))

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