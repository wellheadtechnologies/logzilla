(ns gui.files
  (:use util gui.util gui.las gui.widgets gui.global)
  (:import (org.apache.commons.io FileUtils)
	   (javax.swing JList JFrame DefaultListModel ImageIcon JLabel
			JScrollPane JButton JWindow JPanel SwingUtilities
			JFileChooser JMenu JPopupMenu)
	   (java.io File)
	   (net.miginfocom.swing MigLayout)))

(def file-list (agent []))

(defn get-las-file [name]
  (find-first #(= (.getName %) name) @file-list))

(def file-list-widget 
     (let [jlist (create-jlist)]
       (on-click jlist
	 (fn [e] 
	   (let [name (.. jlist (getSelectedValue) (getText))]
	     (send file-list 
		   (fn [files]
		     (let [file (get-las-file name)]
		       (open-las-view file))
		     files)))))
       (.setOpaque jlist false)
       jlist))

(defn add-las-file [name lasfile]
  (send file-list
	(fn [files]
	  (swing (.. file-list-widget (getModel) (addElement (new JLabel name))))
	  (conj files lasfile)))
  (when *synchronous*
    (await file-list)))

(defn user-selected-files [cwd parent]
  (let [chooser (new JFileChooser cwd)]
    (.setMultiSelectionEnabled chooser true)
    (let [result (.showOpenDialog chooser parent)]
      (if (= JFileChooser/APPROVE_OPTION result)
	(.getSelectedFiles chooser)
	[]))))

(defn create-file-panel []
  (let [outer-panel (create-titled-panel "Las Files")
	inner-panel (new JPanel (new MigLayout))
	scroll-pane (new JScrollPane inner-panel)]
    (doto inner-panel
      (.add file-list-widget "pushx, growx"))
    (doto outer-panel 
      (.add scroll-pane "pushy, grow, pushx, growx"))
    outer-panel))

(def file-panel (create-file-panel))