(ns lasfile.contextmenu.controller
  (:require editor.controller) 
  (:use util gutil global lasfile.contextmenu.view lasfile.model storage)
  (:import (java.awt.event MouseEvent MouseAdapter)))

(defn edit [e]
  (invoke :open-curve-editor))

(defn copy [e]
  (swing-sync
   (let [selected-curve-ids (get-selected-curve-ids)]
     (ref-set copied-curve-ids selected-curve-ids))))

(defn paste [e]
  (swing-sync 
   (let [ccurves @copied-curve-ids]
     (let [lasfile-id @selected-lasfile-id
	   old-lasfile (lookup lasfile-id)
	   old-curves (:curves old-lasfile)
	   new-curves (concat old-curves ccurves)
	   new-lasfile (assoc old-lasfile :curves new-curves)
	   curve-list (get @curve-lists lasfile-id)]
       (change lasfile-id new-lasfile)
       (long-task 
	 (doseq [curve ccurves]
	   (invoke :add-curve curve-list curve)))))))

(defn cremove [e] nil)

(store :context-menu-actions
       {:edit edit
	:copy copy
	:paste paste
	:remove cremove})

(defn init-listener [curve-list]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (let [cm-actions (lookup :context-menu-actions)]
		      (create-context-menu curve-list 
		       (.getX e) (.getY e) cm-actions))))))
