(ns file.filemenu.view
  (:use gutil util global)
  (:import (javax.swing JMenu JFileChooser JPanel 
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

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