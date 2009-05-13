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

;;initializers
(defn- create-manager-widget [source-tree curve-panel]
  (let [panel (JPanel. (MigLayout. "ins 0"))
	source-panel (MacWidgetFactory/createSourceListScrollPane source-tree)]
    (doto panel
      (.add source-panel "width 40%, height 100%")
      (.add curve-panel "width 60%, height 100%"))))

(defn- init-inspector-listener [curve-list]
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

(defn init-file-menu [source-manager]
  (create-file-menu 
   (partial file-menu-open source-manager)
   (partial file-menu-save-all source-manager)
   file-menu-quit))

;;actions
(defn- open-file [file]
  (let [path (if (string? file) file (.getPath file))]
    (try 
     (lasso/load-lasfile path)
     (catch Exception e 
       (JOptionPane/showMessageDialog 
	(:sources-frame @app) (str "There was an error reading file " path)
	"Read Error" JOptionPane/ERROR_MESSAGE)
       (throw e)))))

(defn- open-files [files]
  (doall (map open-file files)))

(defn- save-file [file]
  (let [lasfile (:lasfile @file)]
    (try 
     (lasso/save-lasfile lasfile)
     (catch Exception e
       (JOptionPane/showMessageDialog
	(:sources-frame @app) (str "There was an error saving " (:name @lasfile))
	"Save Error" JOptionPane/ERROR_MESSAGE)
       (throw e)))))

(defn get-selected-source 
  ([] (get-selected-source (:source-manager @app)))
  ([source-manager]
      (:selected-source @source-manager)))

(defn get-selected-curves [curve-list]
  (swing-getter
   (doall (map #(.getCurve %) (.getSelectedValues curve-list)))))

(defn open-curve-editor [source-manager]
  (let [file @(get-selected-source source-manager)
	curve (only (get-selected-curves (:curve-list file)))
	lasfile (:lasfile file)]
    (editor.controller/open-curve-editor lasfile curve)))

(defn open-curve-merger [source-manager]
  (let [file @(get-selected-source source-manager)
	curves (get-selected-curves (:curve-list file))
	lasfile (:lasfile file)]
    (merger.controller/open-curve-merger lasfile curves)))

(defn open-curve-editor-action [source-manager e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (open-curve-editor source-manager)))

(defn display-curves-for [source-manager source]
  (swing-agent
   (dosync (alter source-manager assoc :selected-source source)))
  (let [curve-panel (:curve-panel @source-manager)]
    (doto curve-panel
      (.removeAll)
      (.add (:view @source) "push, grow")
      (.revalidate)
      (.repaint))))

(defn add-curve 
  ([file curve]
     (dosync 
      (let [lasfile (:lasfile @file)
	    curves (:curves @lasfile)
	    curve-list (:curve-list @file)]
	(alter lasfile assoc :curves (conj curves curve))
	(long-task (add-curve-to-gui curve-list curve)))))
  ([curve]
     (let [file (get-selected-source)]
       (add-curve file curve))))

(defmulti open-lasfile (fn [& args]
			 (cond 
			  (and (= 2 (count args)) (string? (second args))) [:source-manager :path]
			  (and (= 2 (count args)) (not (string? (second args)))) [:source-manager :lasfile]
			  (and (= 1 (count args)) (string? (first args))) :path
			  (and (= 1 (count args)) (not (string? (first args)))) :lasfile
			  )))

(defmethod open-lasfile [:source-manager :path] [source-manager path]
  (open-lasfile source-manager (open-file path)))

(defmethod open-lasfile [:source-manager :lasfile] [source-manager lasfile]
  (dosync 
   (let [{:keys [sources source-tree]} @source-manager
	 file (init-file source-manager lasfile)
	 lasfiles-node (.. source-tree (getModel) (getRoot) (getChildAt 0))]
     (alter source-manager assoc :sources (conj sources file))
     (swing-agent
      (doto lasfiles-node
	(.add (DefaultMutableTreeNode. (custom-tree-payload file))))
      (.. source-tree (getModel) (reload lasfiles-node)))
     (when (not @interactive)
       file))))

(defmethod open-lasfile :path [path]
  (open-lasfile (:source-manager @app) path))

(defmethod open-lasfile :lasfile [lasfile]
  (open-lasfile (:source-manager @app) lasfile))
