(ns sources.controller)

(declare open-source open-file save-file)

(defn- create-file-menu [open save-all quit]
  (let [menu (new JMenu "File")]
    (actions menu
      ["Open" open]
      ["Save All" save-all]
      ["Quit" quit])
    menu))

(defn- create-file-selection-dialog [cwd]
  (let [chooser (new JFileChooser cwd)]
    (.setMultiSelectionEnabled chooser true)
    chooser))

(defn- run-file-selection-dialog [cwd]
  (let [frame (:sources-frame @app)
	dialog (create-file-selection-dialog cwd)
	result (.showOpenDialog dialog frame)]
    (if (= JFileChooser/APPROVE_OPTION result)
      (.getSelectedFiles dialog)
      [])))

(defn- file-menu-open [source-manager e]
  (let [files (run-file-selection-dialog ".")]
    (doseq [file files] 
      (long-task (open-source source-manager (open-file file))))))

(defn- file-menu-save-all [source-manager e] 
  (doseq [file (:sources @source-manager)]
    (save-file file)))

(defn- file-menu-quit [e] (System/exit 0))
