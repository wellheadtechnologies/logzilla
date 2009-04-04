(ns lasfile.controller
  (:require editor.controller
	    [lasfile.contextmenu.controller :as cmc]
	    [lasfile.filemenu.controller :as fmc])
  (:use lasfile.view lasfile.model gutil util curves global storage)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel)
	   (java.awt.event MouseEvent MouseAdapter)
	   (javax.swing.event ChangeListener)
	   (gui IconListCellRenderer)))

(declare init-curve-list init-lasfile-view)

(defaction add-lasfile [lasfile-id]
  (dosync 
   (let [lasfile (lookup lasfile-id)
	 curve-list (init-curve-list lasfile)
	 view (init-lasfile-view curve-list)
	 pane @lasfile-pane]
     (alter lasfile-ids conj lasfile-id)
     (alter curve-lists assoc lasfile-id curve-list)
     (swing (.addTab pane (:name lasfile) view)))))

(defaction add-curve [curve-list curve-id]
  (let [curve (lasso/reconstruct-curve (lookup curve-id))
	icon (curve-to-icon curve-id curve)]
    (swing 
     (.addElement (.getModel curve-list) icon)
     (.invalidate curve-list)
     (.repaint curve-list))))

(defaction open-curve-editor []
  (swing-sync
   (let [selected-curve-ids (get-selected-curve-ids)]
     (long-task (editor.controller/open-curve-editor 
		 @selected-lasfile-id selected-curve-ids)))))

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

(defn open-curve-editor-action [e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (invoke :open-curve-editor)))

(defn init-curve-list [lasfile]
  (let [curve-list (create-curve-list)]
    (long-task
      (doseq [curve (:curves lasfile)]
	(invoke :add-curve curve-list curve)))
    (doto curve-list
      (.addMouseListener (click-listener open-curve-editor-action))
      (.addMouseListener (cmc/init-listener curve-list)))))

(defn init-lasfile-view [curve-list]
  (create-lasfile-view curve-list))

(defn init-pane-change-listener []
  (proxy [ChangeListener] []
    (stateChanged [e]
		  (dosync 
		   (let [index (.getSelectedIndex @lasfile-pane)
			 lasfile-id (nth @lasfile-ids index)]
		     (ref-set selected-lasfile-id lasfile-id))))))

(defn init-lasfile-pane []
  (let [pane (create-lasfile-pane)]
    (.addChangeListener pane (init-pane-change-listener))
    (dosync (ref-set lasfile-pane pane))
    pane))

(defn init-file-menu [] (fmc/init-default-menu))
