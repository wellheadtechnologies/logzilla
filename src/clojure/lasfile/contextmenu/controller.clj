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
  (let [{:keys [las-file curve-list]} (state/get-current-view-data)
	selected-curves (state/get-selected-curves curve-list)]
    (when (< 0 (count selected-curves))
      (editor.controller/open-curve-editor las-file selected-curves))))

(defn default-copy-action [e] nil)
(defn default-paste-action [e] nil)
(defn default-remove-action [e] nil)

(defn init-listener [config]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (create-context-menu 
		     (assoc config :x (.getX e) :y (.getY e)))))))

(defn init-default-listener [curve-list]
  (init-listener 
   (struct-map ContextMenuConfig
     :curve-list curve-list
     :edit-action default-edit-action
     :copy-action default-copy-action
     :paste-action default-paste-action
     :remove-action default-remove-action)))