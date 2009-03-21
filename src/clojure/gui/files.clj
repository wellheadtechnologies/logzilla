(ns gui.files
  (:use util gui.util gui.las gui.widgets gui.global)
  (:import (org.apache.commons.io FileUtils)
	   (javax.swing JList JFrame DefaultListModel ImageIcon JLabel
			JScrollPane JButton JWindow JPanel SwingUtilities
			JFileChooser JMenu JPopupMenu)
	   (java.io File)
	   (core DefaultLasParser)
	   (net.miginfocom.swing MigLayout)))

(def file-list (agent []))

(def file-list-widget 
     (let [jlist (create-jlist)]
       (on-click jlist
	 (fn [e] 
	   (let [name (.. jlist (getSelectedValue) (getText))]
	     (send file-list 
		   (fn [flist]
		     (let [file (find-first #(= (.getName %) name) flist)]
		       (open-las-view file))
		     flist)))))
       (.setOpaque jlist false)
       jlist))

(defn add-las-file [name lasfile]
  (send file-list
	(fn [flist]
	  (.. file-list-widget (getModel) (addElement (new JLabel name)))
	  (send las-views assoc lasfile (create-las-view lasfile))
	  (conj flist lasfile)))
  (when *synchronous*
    (await file-list)))

(defn get-las-file [name]
  (find-first #(= (.getName %) name) @file-list))

(defmulti open-file class)

(defmethod open-file File [file]
  (let [lf (DefaultLasParser/parseLasFile file)]
    (add-las-file (.getName file) lf)
    lf))

(defmethod open-file String [path]
  (open-file (new File path)))
    
(defn open-files [files]
  (if *synchronous*
    (for [file files]
      (open-file file))
    (doseq [file files]
      (short-task (open-file file)))))

(defn open-files-in-directory [path]
  (let [directory (new File path)
	files (.listFiles directory)]
    (open-files files)))

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