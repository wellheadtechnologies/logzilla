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

(declare init-curve-list init-curve-list-view
	 init-file init-context-menu-listener
	 display-curves-for)

(load "contextmenu")
(load "file")
(load "curvelist")
(load "filemenu")
(load "sourcetree")

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

(defn create-manager-widget [source-tree curve-panel]
  (let [panel (JPanel. (MigLayout. "ins 0"))
	source-panel (MacWidgetFactory/createSourceListScrollPane source-tree)]
    (doto panel
      (.add source-panel "width 40%, height 100%")
      (.add curve-panel "width 60%, height 100%"))))

(defn init-inspector-listener [curve-list]
  (proxy [ListSelectionListener] []
    (valueChanged [e] (update-parameters-tab :curve (first (get-selected-curves curve-list))))))

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