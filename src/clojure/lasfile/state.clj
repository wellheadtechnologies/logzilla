(ns lasfile.state)

(defstruct LasfileList :pane :view-data) ;pane = [], view-data = [LasViewData]
(defstruct LasViewData :las-file :curve-list)

(def lasfile-list (agent nil)) ; FileList
(def file-menu-config (agent {})) ;FileMenuConfig

(defn get-current-view-data []
  (let [{:keys [pane view-data]} @lasfile-list]
    (nth view-data (.getSelectedIndex pane))))

(defn current-curve-list []
  (let [data (get-current-view-data)]
    (:curve-list data)))

(defn get-selected-curves
  ([] (get-selected-curves (current-curve-list)))
  ([curve-list] (map #(.getCurve %) (.getSelectedValues curve-list))))