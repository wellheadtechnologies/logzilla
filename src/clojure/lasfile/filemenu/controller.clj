(ns lasfile.filemenu.controller
  (:require global app.state)
  (:use lasfile.filemenu.view lasfile.filemenu.model)
  (:import (javax.swing JMenu JFileChooser JPanel 
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(defstruct FileMenuConfig :open-action :save-all-action :quit-action)

(defn run-file-selection-dialog [cwd]
  (let [frame (:frame @app.state/app-config)
	dialog (create-file-selection-dialog cwd)
	result (.showOpenDialog dialog frame)]
    (if (= JFileChooser/APPROVE_OPTION result)
      (.getSelectedFiles dialog)
      [])))

(defn default-open-action [add-lasfile]
  (fn  [e] 
    (let [files (run-file-selection-dialog ".")]
      (doseq [file files]
	(global/long-task 
	 (add-lasfile (open-file file)))))))

(defn default-save-all-action [e] nil)

(defn default-quit-action [e] (System/exit 0))

(defn init-default-menu [add-lasfile]
  (create-file-menu 
   (struct-map FileMenuConfig
     :open-action (default-open-action add-lasfile)
     :save-all-action default-save-all-action
     :quit-action default-quit-action)))