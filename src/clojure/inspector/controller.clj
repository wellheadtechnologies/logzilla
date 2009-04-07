(ns inspector.controller
  (:use util gutil inspector.view))

(defn init-inspector []
  (create-inspector-window 200 400))

(defn open-inspector []
  (init-inspector))