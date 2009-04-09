(ns editor.chart.model
  (:use gutil util)
  (:import (org.jfree.ui RectangleEdge)))

(defn java-2D-to-value [chart-panel x]
  (swing-io! 
   (let [chart (.getChart chart-panel)
	 xaxis (.. chart (getPlot) (getRangeAxis))
	 value (.java2DToValue xaxis x (.getScreenDataArea chart-panel) RectangleEdge/TOP)]
     value)))

(defn retrieve-series [chart-panel]
  (swing-io! 
   (first (.. chart-panel (getChart) (getPlot) (getDataset) (getSeries)))))

(defn get-unit-scale [min-d max-d scale]
  (let [range (abs (- max-d min-d))
	scale (/ range scale)]
    scale))

(defn scale-value [slider-notches scale-notches scale value]
  (let [ratio (/ slider-notches scale-notches)]
    (* (/ value ratio) scale)))

(defstruct Chart
  :editor
  :chart-panel
  :curve
  :dirty-curve
  :scale
  :extent
  :changed-index
  :dragged-entity)