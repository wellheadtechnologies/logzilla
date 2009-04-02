(ns lasfile.controller
  (:require editor.controller
	    [lasfile.contextmenu.controller :as cmc]
	    [lasfile.filemenu.controller :as fmc])
  (:use lasfile.view lasfile.model gutil util curves global)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel)
	   (java.awt.event MouseEvent MouseAdapter)
	   (javax.swing.event ChangeListener)
	   (gui IconListCellRenderer)))

(defn tab-right []
  (swing 
    (let [pane @lasfile-pane
	  index (.getSelectedIndex pane)
	  total (.getTabCount pane)]
      (.setSelectedIndex pane (mod (inc index) total)))))

(defn tab-left []
  (swing 
    (let [pane @lasfile-pane
	  index (.getSelectedIndex pane)
	  total (.getTabCount pane)]
      (.setSelectedIndex pane (mod (dec index) total)))))

(defn add-curve [curve-list curve]
  (let [icon (curve-to-icon curve)]
    (swing 
     (.addElement (.getModel curve-list) icon)
     (.invalidate curve-list)
     (.repaint curve-list))))

(defn open-curve-editor []
  (swing 
    (dosync 
     (let [lasfile @selected-lasfile
	   curve-list (get @curve-lists lasfile)
	   selected-curves (map #(.getCurve %) (.getSelectedValues curve-list))]
       (long-task (editor.controller/open-curve-editor lasfile selected-curves))))))

(defn open-curve-editor-action [e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (open-curve-editor)))

(defn init-curve-list [lasfile]
  (let [curve-list (create-curve-list)]
    (long-task
      (doseq [curve (:curves lasfile)]
	(add-curve curve-list curve)))
    (doto curve-list
      (.addMouseListener (click-listener open-curve-editor-action))
      (.addMouseListener (cmc/init-default-listener curve-list add-curve)))))

(defn init-lasfile-view [lasfile curve-list]
  (create-lasfile-view lasfile curve-list))

(defn add-lasfile [lasfile]
  (dosync 
   (let [curve-list (init-curve-list lasfile)
	 view (init-lasfile-view lasfile curve-list)
	 pane @lasfile-pane]
     (alter lasfile-list conj lasfile)
     (alter curve-lists assoc lasfile curve-list)
     (swing (.addTab pane (:name lasfile) view)))))

(defn init-file-menu [] (fmc/init-default-menu add-lasfile))

(defn init-pane-change-listener []
  (proxy [ChangeListener] []
    (stateChanged [e]
		  (dosync 
		   (let [index (.getSelectedIndex @lasfile-pane)
			 lasfile (nth @lasfile-list index)]
		     (ref-set selected-lasfile lasfile))))))

(defn init-lasfile-pane []
  (let [pane (create-lasfile-pane)]
    (.addChangeListener pane (init-pane-change-listener))
    (dosync (ref-set lasfile-pane pane))
    pane))