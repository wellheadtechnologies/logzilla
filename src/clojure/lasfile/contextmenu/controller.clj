(ns lasfile.contextmenu.controller
  (:require editor.controller)
  (:use util gutil global lasfile.contextmenu.view)
  (:import (java.awt.event MouseEvent MouseAdapter)))

(defstruct ContextMenuConfig
  :component :x :y
  :edit-action
  :copy-action
  :paste-action
  :remove-action)

(defn default-edit-action [e] nil)
(defn default-copy-action [e] nil)
(defn default-paste-action [e] nil)
(defn default-remove-action [e] nil)

(defn init-listener [config]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (create-context-menu 
		     (assoc config :x (.getX e) :y (.getY e)))))))

(defn init-default-listener [component]
  (init-listener 
   (struct-map ContextMenuConfig
     :component component
     :edit-action default-edit-action
     :copy-action default-copy-action
     :paste-action default-paste-action
     :remove-action default-remove-action)))