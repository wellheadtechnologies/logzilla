(ns editor.model
  (:require lasso)
  (:import (org.jfree.ui RectangleEdge)))

;impure
(defn index-to-row [table index] index)

;impure
(defn row-to-index [table row] row)

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
  :height
  :canonical-percentage)
