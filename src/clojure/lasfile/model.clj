(ns lasfile.model
  (:use gutil))

(def lasfile-pane (ref nil)) ;JTabbedPane
(def lasfile-list (ref [])) ; [lasfile]
(def curve-lists (ref {})) ; lasfile -> JList
(def selected-lasfile (ref nil))
(def copied-curves (ref []))

;must occur in swing/dosync
(defn get-selected-curves []
  (let [lasfile @selected-lasfile
	curve-list (get @curve-lists lasfile)
	selected-curves (map #(.getCurve %) (.getSelectedValues curve-list))]
    selected-curves))