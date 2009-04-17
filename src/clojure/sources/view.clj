(ns sources.view
  (:use gutil)
  (:import (javax.swing JMenu JFileChooser JPanel JTree
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane JSplitPane
			JPopupMenu JMenuItem)
	   (java.awt Color)
	   (gui IconListCellRenderer NodePayload)
	   (net.miginfocom.swing MigLayout)
	   (javax.swing.tree DefaultTreeModel)
	   (javax.swing.border BevelBorder)
	   (com.explodingpixels.macwidgets SourceList SourceListModel SourceListCategory 
					   MacWidgetFactory)))

(defn create-curve-list []
  (let [jlist (JList. (DefaultListModel.))]
    (doto jlist
      (.setVisibleRowCount 0)
      (.setBorder (BorderFactory/createEmptyBorder))
      (.setCellRenderer (IconListCellRenderer.))
      (.setBackground (.getBackground (JPanel.)))
      (.setOpaque false))))

(defn create-curve-list-view [curve-list]
  (let [inner-panel (JPanel. (MigLayout.))
	pane (JScrollPane. inner-panel)
	outer-panel (JPanel. (MigLayout.))]
    (doto inner-panel
      (.add curve-list "pushx, growx, pushy, growy, wrap"))
    (doto outer-panel
      (.add pane "pushx, pushy, growx, growy, wrap"))))

(defn create-source-tree []
  (tree 
   ["" 
    ["Las Files"] 
    "Other"]))

(defn create-manager-widget [source-tree]
  (let [panel (JPanel. (MigLayout. "ins 0"))
	source-panel (MacWidgetFactory/createSourceListScrollPane source-tree)
	curve-panel (JPanel. (MigLayout.))]
    (doto panel
      (.add source-panel "width 50%, height 100%")
      (.add curve-panel "width 50%, height 100%"))))

(defn custom-tree-payload [file]
  (proxy [NodePayload] []
    (toString [] 
	      (dosync 
	       (let [lasfile (:lasfile @file)]
		 (:name @lasfile))))
    (getFile [] file)))

;; file-menu
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

;; context-menu

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
