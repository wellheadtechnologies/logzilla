(ns lasfile.controller
  (:load "/global")
  (:load "/editor/controller")
  (:use lasfile.view lasfile.model gutil util curves)
  (:import (javax.swing JFileChooser JLabel JList DefaultListModel)
	   (gui IconListCellRenderer)))

(defstruct LasfileList :pane :list)
(def lasfile-list (agent (struct LasfileList nil []))) ; FileList

(def add-lasfile)
(defn run-file-selection-dialog [cwd]
  (let [frame (:frame @global/app-config)
	dialog (create-file-selection-dialog cwd)
	result (.showOpenDialog dialog frame)]
    (if (= JFileChooser/APPROVE_OPTION result)
      (.getSelectedFiles dialog)
      [])))

;;file actions
(defn open-action [e] 
  (let [lasfiles (open-files (run-file-selection-dialog "."))]
    (doseq [lasfile lasfiles]
      (add-lasfile lasfile))))
(defn save-all-action [e] nil)
(defn quit-action [e] (System/exit 0))

(defn init-file-menu []
  (create-file-menu 
   (struct-map FileMenuConfig
     :open-action open-action
     :save-all-action save-all-action
     :quit-action quit-action)))

(defn add-curve [curve-list curve]
  (let [icon (curve-to-icon curve)]
    (swing (.addElement (.getModel curve-list) icon))))

(defn get-selected-curves [curve-list]
  (map #(.getCurve %) (.getSelectedValues curve-list)))

(defn curve-list-click-action [e]
  (send lasfile-list 
	(fn [{:keys [pane list] :as fl}]
	  (let [selected-curves (get-selected-curves (.getSource e))
		tab-index (.getSelectedIndex pane)
		lasfile (nth list tab-index)]
	    (editor.controller/open-curve-editor lasfile selected-curves))
	  fl)))

(defn init-curve-list [lasfile]
  (let [curve-list (create-curve-list curve-list-click-action)]
    (doseq [curve (:curves lasfile)]
      (add-curve curve-list curve))
    curve-list))

(defn init-lasfile-view [lasfile]
  (create-lasfile-view
   (struct-map LasViewConfig
     :las-file lasfile
     :curve-list (init-curve-list lasfile))))

(defn add-lasfile [lasfile]
  (send lasfile-list
	(fn [{:keys [pane list] :as fl}]
	  (swing (.addTab pane (:name lasfile) (init-lasfile-view lasfile)))
	  (assoc fl :list (conj list lasfile)))))

(defn init-lasfile-pane []
  (let [pane (create-lasfile-pane)]
    (send lasfile-list assoc :pane pane)
    pane))

