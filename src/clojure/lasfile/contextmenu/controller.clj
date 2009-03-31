(ns lasfile.contextmenu.controller
  (:require editor.controller 
	    [lasfile.state :as state] ) 
  (:use util gutil global lasfile.contextmenu.view)
  (:import (java.awt.event MouseEvent MouseAdapter)))

(defstruct ContextMenuConfig
  :curve-list :x :y
  :edit-action
  :copy-action
  :paste-action
  :remove-action)

(defn default-edit-action [e] 
  (let [{:keys [lasfile curve-list]} (state/get-current-view-data)
	selected-curves (state/get-selected-curves curve-list)]
    (when (< 0 (count selected-curves))
      (editor.controller/open-curve-editor lasfile selected-curves))))

(defn default-copy-action [e] 
  (send state/copied-curves (fn [_] (state/get-selected-curves))))

(defn default-paste-action [add-curve]
  (fn [e] 
    (let [ccurves @state/copied-curves]
      (send state/lasfile-list
	    (fn [{:keys [pane view-data-list] :as lfl}]
	      (let [{:keys [lasfile curve-list] :as old-view-data} (nth view-data-list (.getSelectedIndex pane))
		    old-curves (:curves lasfile)
		    new-curves (concat old-curves ccurves)
		    new-lasfile (assoc lasfile :curves new-curves)
		    new-view-data (assoc old-view-data :lasfile new-lasfile)
		    new-data-list (replace {old-view-data new-view-data} view-data-list)]
		(swing 
		 (doseq [curve ccurves]
		   (add-curve curve-list curve)))
		(assoc lfl :view-data-list new-data-list)))))))

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


