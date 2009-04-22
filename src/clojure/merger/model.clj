(ns merger.model
  (:use util)
  (:import (gui ChartUtil)))

(defstruct Merger
  :frame
  :lasfile 
  :index
  :slider
  :charts
  :width
  :height
  :canonical-percentage)

(defn merge-curves [left right]
  {:descriptor (:descriptor @left)
   :index (:index @left)
   :data (ChartUtil/mergeData (:data @left) (:data @right))
   })

(defn zero-out [curve]
  (assoc curve 
    :data (map (fn [_] Double/NaN) (:data curve))))
