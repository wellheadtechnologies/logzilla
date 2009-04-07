(ns editor.model
  (:require lasso)
  (:import (org.jfree.ui RectangleEdge)))

;impure
(defn index-to-row [index table]
  (- (dec (.getRowCount table)) index))

;impure
(defn row-to-index [row table]
  (- (dec (.getRowCount table)) row))

(defstruct Editor
  :frame 
  :lasfile
  :index
  :slider
  :table
  :charts
  :width
  :height)

(defstruct EditorGlobalMethods
  :not-dragging-anything)