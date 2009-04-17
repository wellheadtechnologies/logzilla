(ns sources.view
  (:use gutil)
  (:import (javax.swing JMenu JFileChooser JPanel JTree
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane JSplitPane
			JPopupMenu JMenuItem BorderFactory)
	   (javax.swing.border BevelBorder)
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
    ["Las Files"] 
    "Other"]))

(defn create-curve-panel []
  (let [panel (JPanel. (MigLayout. "ins 0"))]
    (doto panel
      (.setBorder (BorderFactory/createBevelBorder BevelBorder/LOWERED)))))

(defn create-manager-widget [source-tree curve-panel]
  (let [panel (JPanel. (MigLayout. "ins 0"))
	source-panel (MacWidgetFactory/createSourceListScrollPane source-tree)]
    (doto panel
      (.add source-panel "width 35%, height 100%")
      (.add curve-panel "width 65%, height 100%"))))

(defn custom-tree-payload [file]
  (proxy [NodePayload] []
    (toString [] 
	      (dosync 
	       (let [lasfile (:lasfile @file)]
		 (:name @lasfile))))
    (getFile [] file)))

(defn create-file-view [curve-list-view header-edit-button save-lasfile-button]
  (let [panel (JPanel. (MigLayout. "ins 0, nogrid"))]
    (doto panel
      (.add curve-list-view "push, grow, spanx 2, wrap")
      (.add header-edit-button "alignx 50%")
      (.add save-lasfile-button "wrap"))))

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
