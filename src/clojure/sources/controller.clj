(ns sources.controller
  (:require [sources.headerdialog.controller :as header-dialog]
	    editor.controller
	    merger.controller
	    chart.controller
	    chart.render)
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

(load "contextmenu")
(load "file")
(load "curvelist")

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

(defn create-source-tree []
  (tree 
   ["" 
    "Las Files" 
    "Other"]))

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

(defn get-selected-source [source-manager]
  (:selected-source @source-manager))

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

(defn init-inspector-listener [curve-list]
  (proxy [ListSelectionListener] []
    (valueChanged [e] (update-parameters-tab :curve (first (get-selected-curves curve-list))))))

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
(defn init-file-menu [source-manager]
  (create-file-menu 
   (partial file-menu-open source-manager)
   (partial file-menu-save-all source-manager)
   file-menu-quit))

