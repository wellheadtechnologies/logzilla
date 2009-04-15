(ns file.controller
  (:require [file.filemenu.controller :as fmc]
	    [file.contextmenu.controller :as cmc]
	    [file.headerdialog.controller :as header-dialog]
	    editor.controller)
  (:use file.view file.model gutil util curves global)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel JScrollPane
			JSplitPane JTabbedPane JToggleButton JPanel JButton JDialog
			JTable)
	   (java.awt.event MouseEvent MouseAdapter)
	   (java.awt Dimension)
	   (javax.swing.event ChangeListener ListSelectionListener)
	   (javax.swing.table DefaultTableModel)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(declare init-curve-list init-curve-list-view init-file)

(def curve-watcher (agent nil))

(defn update-curve-icon [curve-list old-descriptor curve]
  (dosync 
   (let [icon (:icon @curve)
	 descriptor (:descriptor @curve)]
     (when (not= descriptor old-descriptor)
       (swing 
	(.setText icon (:mnemonic descriptor))
	(.repaint curve-list)))
     descriptor)))

(defn get-selected-curves [curve-list]
  (swing-io! (doall (map #(.getCurve %) (.getSelectedValues curve-list)))))

(defn init-inspector-listener [curve-list]
  (proxy [ListSelectionListener] []
    (valueChanged [e] (switch-inspector-tab :curves (get-selected-curves curve-list)))))

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
     (swing (.addTab pane (:name @lasfile) (:view @file))))))

(defn add-curve-to-gui [curve-list curve]
  (let [icon (curve-to-icon curve)]
    (dosync 
     (alter curve assoc :icon icon)
     (add-watcher curve :send curve-watcher (partial update-curve-icon curve-list))
     (swing 
      (.addElement (.getModel curve-list) icon)
      (.repaint curve-list)))))

(defn add-curve [file curve]
  (dosync 
   (let [lasfile (:lasfile @file)
	 curves (:curves @lasfile)
	 curve-list (:curve-list @file)]
     (alter lasfile assoc :curves (conj curves curve))
     (long-task (add-curve-to-gui curve-list curve)))))

(defn open-curve-editor [file-manager]
  (swing 
   (let [file @(get-selected-file file-manager)
	 curve (only (get-selected-curves (:curve-list file)))
	 lasfile (:lasfile file)]
     (long-task
      (editor.controller/open-curve-editor lasfile curve)))))

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
	    :add-lasfile add-lasfile
	    :open-curve-editor open-curve-editor)))

(defn open-curve-editor-action [file-manager e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1)
	     (= (.getClickCount e) 2))
    (open-curve-editor file-manager)))

(defn init-curve-list [file-manager curves]
  (let [curve-list (create-curve-list)]
    (.addListSelectionListener curve-list (init-inspector-listener curve-list))
    (long-task
      (doseq [curve curves]
	(add-curve-to-gui curve-list curve)))
    (doto curve-list
      (.addMouseListener (click-listener (partial open-curve-editor-action file-manager)))
      (.addMouseListener (cmc/init-listener file-manager curve-list)))
    curve-list))

(defn init-curve-list-view [curve-list]
  (create-curve-list-view curve-list))

(defn init-file-menu [file-manager] (fmc/init-menu file-manager))

(defn init-save-lasfile-button [lasfile]
  (let [button (JButton. "Save Lasfile")]
    (.putClientProperty button "JButton.buttonType" "textured")
    (on-action button
      (lasso/save-lasfile lasfile))
    button))

(defn init-file [file-manager lasfile]
  (let [curve-list (init-curve-list file-manager (:curves @lasfile))
	curve-list-view (create-curve-list-view curve-list)
	panel (JPanel. (MigLayout. "nogrid"))
	edit-headers-button (header-dialog/init-edit-button lasfile)
	save-button (init-save-lasfile-button lasfile)
	file (struct File lasfile curve-list panel)]
    (doto panel
      (.add curve-list-view "push, grow, spanx 2, wrap")
      (.add edit-headers-button "alignx 10%")
      (.add save-button "wrap"))
    (ref file)))

(defn init-file-manager []
  (ref 
   (struct-map FileManager
     :files []
     :pane (create-file-pane))))

