(ns lasfile.filemenu.view
  (:use gutil util global)
  (:import (javax.swing JMenu JFileChooser JPanel 
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(defn create-file-menu [{:keys [open-action save-all-action quit-action] :as file-menu-config}]
  (let [menu (new JMenu "Las")]
    (actions menu
      ["Open" open-action]
      ["Save All" save-all-action]
      ["Quit" quit-action])
    menu))

(defn create-file-selection-dialog [cwd]
  (let [chooser (new JFileChooser cwd)]
    (.setMultiSelectionEnabled chooser true)
    chooser))