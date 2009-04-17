(ns sources.model
  (:use gutil))

(defstruct File
  :lasfile
  :curve-list
  :view)


(defstruct SourceManager
  :sources
  :source-tree
  :pane)

(defstruct SourceManagerGlobalMethods
  :get-selected-source
  :get-selected-curves 
  :add-curve
  :add-lasfile
  :open-curve-editor)


;; file-menu

(defn open-file [file]
  (lasso/load-lasfile (.getPath file)))

(defn open-files [files]
  (doall (map open-file files)))
