(ns editor.model
  (:use storage)
  (:require lasso)
  (:import (org.jfree.ui RectangleEdge)))

;impure
(defn index-to-row [index table]
  (- (dec (.getRowCount table)) index))

;impure
(defn row-to-index [row table]
  (- (dec (.getRowCount table)) row))

