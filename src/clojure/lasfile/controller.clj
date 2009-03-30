(ns lasfile.controller
  (:require editor.controller
	    [lasfile.contextmenu.controller :as cmc]
	    [lasfile.filemenu.controller :as fmc])
  (:use lasfile.view lasfile.model gutil util curves global)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel)
	   (java.awt.event MouseEvent MouseAdapter)
	   (gui IconListCellRenderer)))

(defstruct LasfileList :pane :list)
(defstruct LasViewConfig :las-file :curve-list)

(def lasfile-list (agent (struct LasfileList nil []))) ; FileList

(defn add-curve [curve-list curve]
  (let [icon (curve-to-icon curve)]
    (swing (.addElement (.getModel curve-list) icon))))

(defn get-selected-curves [curve-list]
  (map #(.getCurve %) (.getSelectedValues curve-list)))

(defn curve-list-click-action [e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (send lasfile-list 
	  (fn [{:keys [pane list] :as fl}]
	    (let [selected-curves (get-selected-curves (.getSource e))
		  tab-index (.getSelectedIndex pane)
		  lasfile (nth list tab-index)]
	      (long-task (editor.controller/open-curve-editor lasfile selected-curves)))
	    fl))))

(defn init-curve-list [lasfile]
  (let [curve-list (create-curve-list curve-list-click-action)]
    (global/long-task
     (doseq [curve (:curves lasfile)]
       (add-curve curve-list curve)))
    (doto curve-list
      (.addMouseListener (cmc/init-default-listener curve-list)))
    ))

(defn init-lasfile-view [lasfile]
  (create-lasfile-view
   (struct-map LasViewConfig
     :las-file lasfile
     :curve-list (init-curve-list lasfile))))

(defn add-lasfile [lasfile]
  (send lasfile-list
	(fn [{:keys [pane list] :as fl}]
	  (let [view (init-lasfile-view lasfile)]
	    (swing (.addTab pane (:name lasfile) view))
	    (assoc fl :list (conj list lasfile))))))

(defn init-lasfile-pane []
  (let [pane (create-lasfile-pane)]
    (send lasfile-list assoc :pane pane)
    pane))

(defn init-file-menu [] (fmc/init-default-menu add-lasfile))