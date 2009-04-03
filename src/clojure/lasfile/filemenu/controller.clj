(ns lasfile.filemenu.controller
  (:require app.state)
  (:use lasfile.filemenu.view lasfile.filemenu.model global storage)
  (:import (javax.swing JMenu JFileChooser JPanel 
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(def add-lasfile (partial invoke :add-lasfile))

(defn run-file-selection-dialog [cwd]
  (let [frame (:frame @app.state/app-config)
	dialog (create-file-selection-dialog cwd)
	result (.showOpenDialog dialog frame)]
    (if (= JFileChooser/APPROVE_OPTION result)
      (.getSelectedFiles dialog)
      [])))

(defn open [e]
  (let [files (run-file-selection-dialog ".")]
    (doseq [file files] 
      (long-task (add-lasfile (open-file file))))))

(defn save-all [e] nil)

(defn quit [e] (System/exit 0))

(store :file-menu-config
       {:open open
	:save-all save-all
	:quit quit})

(defn init-default-menu []
  (create-file-menu))