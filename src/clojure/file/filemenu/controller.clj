(ns file.filemenu.controller
  (:use file.filemenu.view file.filemenu.model global)
  (:import (javax.swing JMenu JFileChooser JPanel 
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(defn run-file-selection-dialog [cwd]
  (let [frame (:frame @app)
	dialog (create-file-selection-dialog cwd)
	result (.showOpenDialog dialog frame)]
    (if (= JFileChooser/APPROVE_OPTION result)
      (.getSelectedFiles dialog)
      [])))

(defn open [file-manager e]
  (let [files (run-file-selection-dialog ".")]
    (doseq [file files] 
      (long-task (fm-invoke :add-lasfile file-manager (open-file file))))))

(defn save-all [e] nil)

(defn quit [e] (System/exit 0))

(defn init-menu [file-manager]
  (create-file-menu 
   (partial open file-manager)
   save-all
   quit))