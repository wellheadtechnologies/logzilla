(ns sources.controller
  (:require [sources.headerdialog.controller :as header-dialog]
	    editor.controller
	    merger.controller
	    chart.controller)
  (:use gutil util global inspector.controller)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel JScrollPane
			JSplitPane JTabbedPane JToggleButton JPanel JButton JDialog
			JTable JPopupMenu JMenuItem BorderFactory JTree JMenu
			JOptionPane TransferHandler)
	   (java.awt.event MouseEvent MouseAdapter)
	   (java.awt.datatransfer Transferable DataFlavor)
	   (java.awt Dimension)
	   (java.awt.dnd DragSourceAdapter)
	   (javax.swing.event ChangeListener ListSelectionListener TreeSelectionListener)
	   (javax.swing.table DefaultTableModel)
	   (javax.swing.tree DefaultMutableTreeNode TreeCellRenderer)
	   (gui IconListCellRenderer NodePayload CustomTransferHandler Dragger)
	   (net.miginfocom.swing MigLayout)
	   (javax.swing.border BevelBorder)
	   (javax.swing.tree DefaultTreeModel)
	   (com.explodingpixels.macwidgets SourceList SourceListModel 
					   SourceListCategory 
					   MacWidgetFactory)))

(declare init-curve-list init-curve-list-view init-file init-context-menu-listener)

(defstruct File
  :lasfile
  :curve-list
  :view)

(defstruct SourceManager
  :sources
  :selected-source
  :source-tree
  :curve-panel
  :widget)

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

(defn make-transferable [curve]
  (proxy [Transferable] []
    (getTransferData [flavor] curve)
    (getTransferDataFlavors [] 
			    (let [flavors (into-array DataFlavor [ref-data-flavor])]
			      flavors))
    (isDataFlavorSupported [flavor] false)))

(defn create-transfer-handler []
  (proxy [CustomTransferHandler] []
    (createTransferable [c] 
			(make-transferable (first (.getSelectedValues c))))
    (getSourceActions [c] TransferHandler/COPY)))

(defn create-curve-list []
  (let [jlist (JList. (DefaultListModel.))
	dragger (proxy [Dragger] [jlist]
		  (createTransferable [c]
				      (make-transferable (first (.getSelectedValues c))))
		  (createIcon [c] (.. (first (.getSelectedValues c)) (getIcon) (getImage))))]
    (doto jlist
      (.setVisibleRowCount 0)
      (.setBorder (BorderFactory/createEmptyBorder))
      (.setCellRenderer (IconListCellRenderer.))
      (.setBackground (.getBackground (JPanel.)))
      (.setOpaque false))))

(defn create-curve-list-view [curve-list]
  (let [inner-panel (JPanel. (MigLayout. "ins 0"))
	pane (JScrollPane. inner-panel)
	outer-panel (JPanel. (MigLayout. "ins 0"))]
    (doto inner-panel
      (.add curve-list "pushx, growx, pushy, growy, wrap"))
    (doto outer-panel
      (.add pane "pushx, pushy, growx, growy, wrap"))))

(defn create-source-tree []
  (tree 
   ["" 
    "Las Files" 
    "Other"]))

(defn create-curve-panel []
  (let [panel (JPanel. (MigLayout. "ins 0"))]
    (doto panel
      (.setBorder (BorderFactory/createBevelBorder BevelBorder/LOWERED)))))

(defn create-manager-widget [source-tree curve-panel]
  (let [panel (JPanel. (MigLayout. "ins 0"))
	source-panel (MacWidgetFactory/createSourceListScrollPane source-tree)]
    (doto panel
      (.add source-panel "width 40%, height 100%")
      (.add curve-panel "width 60%, height 100%"))))

(defn custom-tree-payload [file]
  (proxy [NodePayload] []
    (toString [] 
	      (dosync 
	       (let [lasfile (:lasfile @file)]
		 (:name @lasfile))))
    (getFile [] file)))

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

(defn create-context-menu [curve-list x y cm-actions]
  (let [m (JPopupMenu.)
	edit (JMenuItem. "Edit")
	merge (JMenuItem. "Merge")
	copy (JMenuItem. "Copy")
	paste (JMenuItem. "Paste")
	remove (JMenuItem. "Remove")]

    (swing
     (set-action edit (:edit cm-actions))
     (set-action merge (:merge cm-actions))
     (set-action copy (:copy cm-actions))
     (set-action paste (:paste cm-actions))
     (set-action remove (:remove cm-actions))
     
     (let [svc (count (.getSelectedValues curve-list))]
       (cond 
	(= 0 svc)
	(do 
	  (.setEnabled edit false)
	  (.setEnabled merge false))
	
	(= 1 svc)
	(do 
	  (.setEnabled edit true)
	  (.setEnabled merge false))
	
	(< 1 svc)
	(do 
	  (.setEnabled edit false)
	  (.setEnabled merge true))))

     (doto m
       (.add edit)
       (.add merge)
       (.add copy)
       (.add paste)
       (.add remove)
       (.show curve-list x y)))))

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
	(.add (DefaultMutableTreeNode. (custom-tree-payload file))))
      (.. source-tree (getModel) (reload lasfiles-node))))))

(defn add-curve-to-gui [curve-list curve]
  (let [icon (chart.controller/curve-to-icon curve)]
    (dosync 
     (alter curve assoc :icon icon)
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

(defn open-curve-merger [source-manager]
  (swing
   (let [file @(get-selected-source source-manager)
	 curves (get-selected-curves (:curve-list file))
	 lasfile (:lasfile file)]
     (long-task
      (merger.controller/open-curve-merger lasfile curves)))))

(defn open-curve-editor-action [source-manager e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (open-curve-editor source-manager)))

(defn init-inspector-listener [curve-list]
  (proxy [ListSelectionListener] []
    (valueChanged [e] (update-parameters-tab :curve (first (get-selected-curves curve-list))))))

(defn init-curve-list [source-manager curves]
  (let [curve-list (create-curve-list)]
    (long-task
      (doseq [curve curves]
	(add-curve-to-gui curve-list curve)))
    (doto curve-list
      (.addMouseListener (click-listener (partial open-curve-editor-action source-manager)))
      (.addMouseListener (init-context-menu-listener source-manager curve-list))
      (.addListSelectionListener (init-inspector-listener curve-list)))
    curve-list))

(defn init-curve-list-view [curve-list]
  (create-curve-list-view curve-list))

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

(defn display-curves-for [source-manager source]
  (dosync 
   (alter source-manager assoc :selected-source source)
   (swing 
    (let [curve-panel (:curve-panel @source-manager)]
      (doto curve-panel
	(.removeAll)
	(.add (:view @source) "push, grow")
	(.revalidate)
	(.repaint))))))

(defn init-source-tree-selection-listener [source-manager]
  (proxy [TreeSelectionListener] []
    (valueChanged [e]
		  (let [path (.getNewLeadSelectionPath e)]
		    (when path
		      (let [leaf (.getLastPathComponent path)
			    payload (.getUserObject leaf)
			    file (.getFile payload)]
			(display-curves-for source-manager file)
			(update-log-tab (:lasfile @file))))))))


(defn init-source-tree [source-manager]
  (let [source-tree (create-source-tree)
	renderer (.getCellRenderer source-tree)]
    (doto source-tree
      (.addTreeSelectionListener (init-source-tree-selection-listener source-manager)))))

(defn init-source-manager []
  (let [source-manager (ref nil)
	source-tree (init-source-tree source-manager)
	curve-panel (create-curve-panel)]
    (dosync 
     (ref-set source-manager
	      (struct-map SourceManager
		:sources []
		:source-tree source-tree
		:curve-panel curve-panel
		:widget (create-manager-widget source-tree curve-panel))))
    source-manager))

(defn run-file-selection-dialog [cwd]
  (let [frame (:sources-frame @app)
	dialog (create-file-selection-dialog cwd)
	result (.showOpenDialog dialog frame)]
    (if (= JFileChooser/APPROVE_OPTION result)
      (.getSelectedFiles dialog)
      [])))

(defn file-menu-open [source-manager e]
  (let [files (run-file-selection-dialog ".")]
    (doseq [file files] 
      (long-task (add-lasfile source-manager (open-file file))))))

(defn file-menu-save-all [source-manager e] 
  (doseq [file (:sources @source-manager)]
    (save-file file)))

(defn file-menu-quit [e] (System/exit 0))

(defn init-file-menu [source-manager]
  (create-file-menu 
   (partial file-menu-open source-manager)
   (partial file-menu-save-all source-manager)
   file-menu-quit))

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