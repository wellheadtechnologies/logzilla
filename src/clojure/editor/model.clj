(ns editor.model
  (:require lasso)
  (:import (org.jfree.ui RectangleEdge)))

;impure
(defn index-to-row [index table]
  (- (dec (.getRowCount table)) index))

;impure
(defn row-to-index [row table]
  (- (dec (.getRowCount table)) row))

(defn convert-to-double [value]
  (if (string? value)
    (Double/valueOf value)
    (double value)))

(defstruct Editor
  :frame 
  :lasfile
  :index
  :slider
  :table
  :chart
  :width
  :height)
