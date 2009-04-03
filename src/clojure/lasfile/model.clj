(ns lasfile.model
  (:use gutil))

(def lasfile-pane (ref nil)) ;JTabbedPane
(def lasfile-ids (ref [])) ; [lasfile-ids]
(def curve-lists (ref {})) ; id -> JList
(def selected-lasfile-id (ref nil))
(def copied-curve-ids (ref []))

;must occur in swing/dosync
(defn get-selected-curve-list []
  (let [lasfile-id @selected-lasfile-id
	curve-list (get @curve-lists lasfile-id)]
    curve-list))

;must occur in swing/dosync
(defn get-selected-curve-ids []
  (let [curve-list (get-selected-curve-list)
	selected-curve-ids (map #(.getCurveID %) (.getSelectedValues curve-list))]
    selected-curve-ids))