(ns file.model
  (:use gutil))

(defstruct File
  :lasfile
  :curve-list
  :view)


(defstruct FileManager
  :files 
  :pane)

(defstruct FileManagerGlobalMethods
  :get-selected-file
  :get-selected-curves
  :add-curve
  :add-lasfile)
