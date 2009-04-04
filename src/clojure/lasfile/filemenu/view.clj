(ns lasfile.filemenu.view
  (:use gutil util global storage)
  (:import (javax.swing JMenu JFileChooser JPanel 
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(defn create-file-menu []
  (let [menu (new JMenu "File")]
    (actions menu
      ["Open" (:open (lookup :file-menu-config))]
      ["Save All" (:save-all (lookup :file-menu-config))]
      ["Quit" (:quit-action (lookup :file-menu-config))])
    menu))

(defn create-file-selection-dialog [cwd]
  (let [chooser (new JFileChooser cwd)]
    (.setMultiSelectionEnabled chooser true)
    chooser))