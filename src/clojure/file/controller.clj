(ns file.controller
  (:require [file.filemenu.controller :as fmc]
	    [file.contextmenu.controller :as cmc])
  (:use file.view file.model gutil util curves global)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel)
	   (java.awt.event MouseEvent MouseAdapter)
	   (javax.swing.event ChangeListener)
	   (gui IconListCellRenderer)))

(declare init-curve-list init-lasfile-view init-file)

(defn get-selected-curves [curve-list]
  (swing-io! (doall (map #(.getCurve %) (.getSelectedValues curve-list)))))

(defn get-selected-file [file-manager]
  (swing-io! 
   (let [pane (:pane @file-manager)
	 files (:files @file-manager)
	 index (.getSelectedIndex pane)]
     (nth files index))))

(defn add-lasfile [file-manager lasfile]
  (dosync 
   (let [files (:files @file-manager)
	 pane (:pane @file-manager)
	 file (init-file file-manager lasfile)]
     (alter file-manager assoc :files (conj files file))
     (println "view = " (:view @file))
     (swing (.addTab pane (:name @lasfile) (:view @file))))))

(defn add-curve-to-gui [curve-list curve]
  (let [icon (curve-to-icon curve)]
    (swing 
     (.addElement (.getModel curve-list) icon)
     (.repaint curve-list))))

(defn add-curve [file curve-ref]
  (dosync 
   (let [lasfile (:lasfile @file)
	 curve-list (:curve-list @file)
	 curve @curve-ref]
     (alter lasfile assoc :curves (conj (:curves lasfile) curve-ref))
     (long-task (add-curve-to-gui curve-list curve)))))

(defn open-curve-editor [file-ref]
  (swing 
   (let [file @file-ref
	 selected-curve-ids (get-selected-curves (:curve-list file))
	 selected-lasfile (:lasfile file)]
     (long-task nil
;(editor.controller/open-curve-editor selected-lasfile selected-curves)
		))))

(defn tab-right [file-manager]
  (swing 
    (let [pane (get @file-manager :pane)
	  index (.getSelectedIndex pane)
	  total (.getTabCount pane)]
      (.setSelectedIndex pane (mod (inc index) total)))))

(defn tab-left [file-manager]
  (swing 
   (let [pane (get @file-manager :pane)
	 index (.getSelectedIndex pane)
	 total (.getTabCount pane)]
     (.setSelectedIndex pane (mod (dec index) total)))))

(dosync 
 (ref-set file-methods 
	  (struct-map FileManagerGlobalMethods
	    :get-selected-file get-selected-file
	    :get-selected-curves get-selected-curves
	    :add-curve add-curve
	    :add-lasfile add-lasfile)))

(defn open-curve-editor-action [file-manager]
  (fn [e]
    (when (and (= (.getButton e) MouseEvent/BUTTON1)
	       (= (.getClickCount e) 2))
      (open-curve-editor file-manager))))

(defn init-curve-list [file-manager curves]
  (let [curve-list (create-curve-list)]
    (println "init curve-list with curves = " (count curves))
    (long-task
      (doseq [curve curves]
	(println "adding curve " (:descriptor @curve))
	(add-curve-to-gui curve-list @curve)))
    (doto curve-list
      (.addMouseListener (click-listener open-curve-editor-action))
      (.addMouseListener (cmc/init-listener file-manager curve-list)))
    curve-list))

(defn init-lasfile-view [curve-list]
  (create-lasfile-view curve-list))

(defn init-file-menu [file-manager] (fmc/init-menu file-manager))

(defn init-file [file-manager lasfile]
  (let [curve-list (init-curve-list file-manager (:curves @lasfile))
	view (create-lasfile-view curve-list)
	file (struct File lasfile curve-list view)]
    (ref file)))

(defn init-file-manager []
  (ref 
   (struct-map FileManager
     :files []
     :pane (create-lasfile-pane))))

