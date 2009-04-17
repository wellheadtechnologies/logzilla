(ns sources.controller
  (:require [sources.headerdialog.controller :as header-dialog]
	    editor.controller)
  (:use sources.view sources.model gutil util curves global)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel JScrollPane
			JSplitPane JTabbedPane JToggleButton JPanel JButton JDialog
			JTable)
	   (java.awt.event MouseEvent MouseAdapter)
	   (java.awt Dimension)
	   (javax.swing.event ChangeListener ListSelectionListener)
	   (javax.swing.table DefaultTableModel)
	   (javax.swing.tree DefaultMutableTreeNode)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(declare init-curve-list init-curve-list-view init-file)

(def curve-watcher (agent nil))

(defn update-curve-icon [curve-list old-descriptor curve]
  (dosync 
   (let [icon (:icon @curve)
	 descriptor (:descriptor @curve)]
     (when (not= descriptor old-descriptor)
       (swing 
	(.setText icon (:mnemonic descriptor))
	(.repaint curve-list)))
     descriptor)))

(defn get-selected-curves [curve-list]
  (swing-io! (doall (map #(.getCurve %) (.getSelectedValues curve-list)))))

(defn init-inspector-listener [curve-list]
  (proxy [ListSelectionListener] []
    (valueChanged [e] (switch-inspector-tab :curves (get-selected-curves curve-list)))))

(defn get-selected-source [source-manager]
  (:selected-source @source-manager))

(defn add-lasfile [source-manager lasfile]
  (dosync 
   (let [{:keys [sources source-tree]} @source-manager
	 file (init-file source-manager lasfile)
	 lasfiles-node (.. source-tree (getModel) (getRoot) (getChildAt 0))]
     (alter source-manager assoc :sources (conj sources file))
     (swing 
      (doto lasfiles-node
	(.add (MutableTreeNode. (:name @lasfile))))))))

(defn add-curve-to-gui [curve-list curve]
  (let [icon (curve-to-icon curve)]
    (dosync 
     (alter curve assoc :icon icon)
     (add-watcher curve :send curve-watcher (partial update-curve-icon curve-list))
     (swing 
      (.addElement (.getModel curve-list) icon)
      (.repaint curve-list)))))

(defn add-curve [file curve]
  (dosync 
   (let [lasfile (:lasfile @file)
	 curves (:curves @lasfile)
	 curve-list (:curve-list @file)]
     (alter lasfile assoc :curves (conj curves curve))
     (long-task (add-curve-to-gui curve-list curve)))))

(defn open-curve-editor [source-manager]
  (swing 
   (let [file @(get-selected-source source-manager)
	 curve (only (get-selected-curves (:curve-list file)))
	 lasfile (:lasfile file)]
     (long-task
      (editor.controller/open-curve-editor lasfile curve)))))

(defn open-curve-editor-action [source-manager e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (open-curve-editor source-manager)))

(defn init-curve-list [source-manager curves]
  (let [curve-list (create-curve-list)]
    (.addListSelectionListener curve-list (init-inspector-listener curve-list))
    (long-task
      (doseq [curve curves]
	(add-curve-to-gui curve-list curve)))
    (doto curve-list
      (.addMouseListener (click-listener (partial open-curve-editor-action source-manager)))
      (.addMouseListener (cmc/init-listener source-manager curve-list)))
    curve-list))

(defn init-curve-list-view [curve-list]
  (create-curve-list-view curve-list))

(defn init-file-menu [source-manager] (fmc/init-menu source-manager))

(defn init-save-lasfile-button [lasfile]
  (let [button (JButton. "Save Lasfile")]
    (.putClientProperty button "JButton.buttonType" "textured")
    (on-action button
      (lasso/save-lasfile lasfile))
    button))

(defn init-file [source-manager lasfile]
  (let [curve-list (init-curve-list source-manager (:curves @lasfile))
	curve-list-view (create-curve-list-view curve-list)
	panel (JPanel. (MigLayout. "nogrid"))
	edit-headers-button (header-dialog/init-edit-button lasfile)
	save-button (init-save-lasfile-button lasfile)
	file (struct File lasfile curve-list panel)]
    (doto panel
      (.add curve-list-view "push, grow, spanx 2, wrap")
      (.add edit-headers-button "alignx 10%")
      (.add save-button "wrap"))
    (ref file)))

(defn init-source-manager []
  (ref 
   (let [source-tree (create-source-tree)]
     (struct-map SourceManager
       :files []
       :source-tree source-tree
       :widget (create-manager-widget source-tree)))))

;;file-menu 

(defn run-file-selection-dialog [cwd]
  (let [frame (:frame @app)
	dialog (create-file-selection-dialog cwd)
	result (.showOpenDialog dialog frame)]
    (if (= JFileChooser/APPROVE_OPTION result)
      (.getSelectedFiles dialog)
      [])))

(defn file-menu-open [source-manager e]
  (let [files (run-file-selection-dialog ".")]
    (doseq [file files] 
      (long-task (add-lasfile source-manager (open-file file))))))

(defn file-menu-save-all [e] nil)

(defn file-menu-quit [e] (System/exit 0))

(defn init-file-menu [source-manager]
  (create-file-menu 
   (partial file-menu-open source-manager)
   file-menu-save-all
   file-menu-quit))

;; context-menu

(defn context-menu-edit [source-manager] 
  (open-curve-editor source-manager))

(defn context-menu-merge [source-manager]
  (open-curve-merger source-manager))

(defn context-menu-copy [source-manager]
  (swing
   (let [file (get-selected-source source-manager)
	 curves (get-selected-curves (:curve-list @file))]
     (dosync (ref-set copied-curves curves)))))

(defn context-menu-paste [source-manager]
  (swing
   (let [ccurves @copied-curves
	 file (get-selected-source source-manager)]
     (long-task 
      (doseq [curve ccurves]
	(add-curve file curve))))))

(defn context-menu-remove [source-manager] nil)

(defn init-context-menu-listener [source-manager curve-list]
  (proxy [MouseAdapter] []
    (mouseClicked [e] 
		  (when (= (.getButton e) MouseEvent/BUTTON3)
		    (create-context-menu
		     curve-list (.getX e) (.getY e) 
		     {:edit (partial context-menu-edit source-manager)
		      :merge (partial context-menu-merge source-manager)
		      :copy (partial context-menu-copy source-manager)
		      :paste (partial context-menu-paste source-manager)
		      :remove (partial context-menu-remove source-manager)
		      })))))

