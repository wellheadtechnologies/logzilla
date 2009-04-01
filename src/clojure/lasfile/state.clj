(ns lasfile.state
  (:use gutil))

(defstruct LasfileList :pane :view-data-list) ;pane = [], view-data = [LasViewData]
(defstruct LasViewData :lasfile :curve-list)

(def lasfile-list (agent nil)) ; FileList
(def file-menu-config (agent {})) ;FileMenuConfig
(def copied-curves (agent []))

(defn get-current-pane []
  (:pane @lasfile-list))

(defn get-current-view-data []
  (let [{:keys [pane view-data-list]} @lasfile-list]
    (nth view-data-list (.getSelectedIndex pane))))

(defn current-curve-list []
  (let [data (get-current-view-data)]
    (:curve-list data)))

(defn get-selected-curves
  ([] (get-selected-curves (current-curve-list)))
  ([curve-list] (map #(.getCurve %) (.getSelectedValues curve-list))))