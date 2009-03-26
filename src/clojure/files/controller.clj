(ns files.controller
  (:load "view")
  (:use files.model util gutil global las.controller)
  (:import (javax.swing JLabel JFileChooser)))

(def file-list (agent []))

(defn run-file-selection-dialog [cwd]
    (let [dialog (files.view/create-file-selection-dialog cwd)
	  result (.showOpenDialog dialog)]
      (if (= JFileChooser/APPROVE_OPTION result)
	(.getSelectedFiles dialog)
	[])))

(defn get-las-file [name]
  (find-first #(= (.getName %) name) @file-list))

(defn add-las-file [name lasfile]
  (send file-list
	(fn [files]
	  (swing (.. files.view/file-list-widget (getModel) (addElement (new JLabel name))))
	  (conj files lasfile)))
  (when *synchronous*
    (await file-list)))

(defn open-action [e]
  (open-files (run-file-selection-dialog ".") add-las-file))

(defn save-all-action [e] nil)

(defn quit-action [e] (System/exit 0))

(defn on-file-list-click [e] 
  (let [name (.. files.view/file-list-widget (getSelectedValue) (getText))]
    (send file-list 
	  (fn [files]
	    (let [file (get-las-file name)]
	      (open-las-view file))
	    files))))

(defn init-file-panel []
  (let [panel (create-file-panel)]
    panel))
