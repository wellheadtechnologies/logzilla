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

;;BEGIN FUNCTIONS
(defn add-lasfile [lasfile-id]
  (dosync 
   (let [lasfile (lookup lasfile-id)
	 curve-list (init-curve-list lasfile)
	 view (init-lasfile-view lasfile curve-list)
	 pane @lasfile-pane]
     (alter lasfile-ids conj lasfile-id)
     (alter curve-lists assoc lasfile-id curve-list)
     (swing (.addTab pane (:name lasfile) view)))))

(defn add-curve [curve-list curve-id]
  (let [curve (lasso/reconstruct-curve (lookup curve-id))
	icon (curve-to-icon curve-id curve)]
    (swing 
     (.addElement (.getModel curve-list) icon)
     (.invalidate curve-list)
     (.repaint curve-list))))

(defstore :open-curve-editor []
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
;;END FUNCTIONS

;;BEGIN ACTIONS
(defn open-curve-editor-action [e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (invoke :open-curve-editor)))
;;END ACTIONS


;;BEGIN INTIALIZERS 
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

(defn init-file-menu [] (fmc/init-default-menu add-lasfile))
;;END INITIALIZERS