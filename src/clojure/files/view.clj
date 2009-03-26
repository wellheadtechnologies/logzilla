(ns files.view
  (:use gutil)
  (:import (org.apache.commons.io FileUtils)
	   (javax.swing JList JFrame DefaultListModel ImageIcon JLabel
			JScrollPane JButton JWindow JPanel SwingUtilities
			JFileChooser JMenu JPopupMenu)
	   (java.io File)
	   (net.miginfocom.swing MigLayout)))

(def open-action)
(def save-all-action)
(def quit-action)
(def file-list-on-click)
(def file-list-widget)
(def file-menu)
(def file-panel)

(defn create-file-list-widget  []
  (let [jlist (create-jlist)]
    (on-click jlist file-list-on-click)
    (.setOpaque jlist false)
    jlist))

(defn create-file-panel []
  (let [outer-panel (create-titled-panel "Las Files")
	inner-panel (new JPanel (new MigLayout))
	scroll-pane (new JScrollPane inner-panel)]
    (doto inner-panel
      (.add file-list-widget "pushx, growx"))
    (doto outer-panel 
      (.add scroll-pane "pushy, grow, pushx, growx"))
    outer-panel))

(defn create-file-menu []
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
