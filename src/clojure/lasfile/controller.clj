(ns lasfile.controller
  (:require editor.controller
	    [lasfile.contextmenu.controller :as cmc]
	    [lasfile.filemenu.controller :as fmc]
	    [lasfile.state :as state])
  (:use lasfile.view lasfile.model gutil util curves global)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel)
	   (java.awt.event MouseEvent MouseAdapter)
	   (javax.swing.event ChangeListener)
	   (gui IconListCellRenderer)))

(defn add-curve [curve-list curve]
  (let [icon (curve-to-icon curve)]
    (swing 
     (.addElement (.getModel curve-list) icon)
     (.invalidate curve-list)
     (.repaint curve-list))))

(defn open-curve-editor-action [e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (let [{:keys [lasfile curve-list]} (state/get-current-view-data)
	  selected-curves (state/get-selected-curves curve-list)]
      (long-task (editor.controller/open-curve-editor lasfile selected-curves)))))

(defn init-curve-list [lasfile]
  (let [curve-list (create-curve-list)]
    (global/long-task
     (doseq [curve (:curves lasfile)]
       (add-curve curve-list curve)))
    (doto curve-list
      (.addMouseListener (click-listener open-curve-editor-action))
      (.addMouseListener (cmc/init-default-listener curve-list add-curve)))))

(defn init-lasfile-view [lasfile]
  (let [data (struct-map state/LasViewData
	       :lasfile lasfile
	       :curve-list (init-curve-list lasfile))]
    [(create-lasfile-view data) 
     data]))

(defn add-lasfile [lasfile]
  (send state/lasfile-list
	(fn [{:keys [pane view-data-list] :as fl}]
	  (let [[view data] (init-lasfile-view lasfile)]
	    (swing (.addTab pane (:name lasfile) view))
	    (assoc fl :view-data-list (conj view-data-list data))))))

(defn init-lasfile-pane []
  (let [pane (create-lasfile-pane)]
    (send state/lasfile-list (fn [_] (struct-map state/LasfileList :pane pane :view-data-list [])))
    pane))

(defn init-file-menu [] (fmc/init-default-menu add-lasfile))