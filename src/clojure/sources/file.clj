(ns sources.controller)

(declare create-curve-list-view File custom-tree-payload)

(defn create-file-view [curve-list-view header-edit-button]
  (let [panel (JPanel. (MigLayout. "ins 0, nogrid"))]
    (doto panel
      (.add curve-list-view "push, grow, spanx 2, wrap")
      (.add header-edit-button "alignx 50%, wrap"))))

(defn create-file-menu [open save-all quit]
  (let [menu (new JMenu "File")]
    (actions menu
      ["Open" open]
      ["Save All" save-all]
      ["Quit" quit])
    menu))

(defn create-file-selection-dialog [cwd]
  (let [chooser (new JFileChooser cwd)]
    (.setMultiSelectionEnabled chooser true)
    chooser))

(defn init-file [source-manager lasfile]
  (let [curve-list (init-curve-list source-manager (:curves @lasfile))
	curve-list-view (create-curve-list-view curve-list)
	edit-headers-button (header-dialog/init-edit-button lasfile)
	panel (create-file-view curve-list-view edit-headers-button)
	file (struct-map File
	       :lasfile lasfile
	       :curve-list curve-list
	       :view panel)]
    (ref file)))

(defn add-lasfile [source-manager lasfile]
  (dosync 
   (let [{:keys [sources source-tree]} @source-manager
	 file (init-file source-manager lasfile)
	 lasfiles-node (.. source-tree (getModel) (getRoot) (getChildAt 0))]
     (alter source-manager assoc :sources (conj sources file))
     (swing 
      (doto lasfiles-node
	(.add (DefaultMutableTreeNode. (custom-tree-payload file))))
      (.. source-tree (getModel) (reload lasfiles-node)))
     file)))

(defn open-file [file]
  (try 
   (lasso/load-lasfile (.getPath file))
   (catch Exception e 
     (JOptionPane/showMessageDialog 
      (:sources-frame @app) (str "There was an error reading file " (.getPath file))
      "Read Error" JOptionPane/ERROR_MESSAGE)
     (throw e))))

(defn open-files [files]
  (doall (map open-file files)))

(defn save-file [file]
  (let [lasfile (:lasfile @file)]
    (try 
     (lasso/save-lasfile lasfile)
     (catch Exception e
       (JOptionPane/showMessageDialog
	(:sources-frame @app) (str "There was an error saving " (:name @lasfile))
	"Save Error" JOptionPane/ERROR_MESSAGE)
       (throw e)))))

