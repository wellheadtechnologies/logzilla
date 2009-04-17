(ns sources.model
  (:use gutil))

(defstruct File
  :lasfile
  :curve-list
  :view)


(defstruct SourceManager
  :sources
  :selected-source
  :source-tree
  :pane)

;; file-menu

(defn open-file [file]
  (lasso/load-lasfile (.getPath file)))

(defn open-files [files]
  (doall (map open-file files)))
