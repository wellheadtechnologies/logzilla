(ns inspector.controller
  (:use util gutil storage inspector.view))

(store :inspector-window-properties
       {:width 200
	:height 400
	:app-tab {:title "App"}})

(defn open-inspector []
  (create-inspector-window (lookup :inspector-window-properties)))