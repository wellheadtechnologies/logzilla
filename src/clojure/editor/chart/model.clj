(ns editor.chart.model
  (:use gutil util curves)
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


(defn get-chart-range [xaxis]
  (let [range (.getRange xaxis)
	lower (.getLowerBound range)
	upper (.getUpperBound range)]
    (abs (- upper lower))))

(defn get-depth-range [curve]
  (let [mind (min-depth curve)
	maxd (max-depth curve)]
    (abs (- maxd mind))))

(defn get-scale [depth-range chart-range]
  (/ depth-range chart-range))

(defn get-unit [depth-range scale]
  (/ depth-range scale))

(defn get-extent [xaxis]
  (.. xaxis (getRange) (getLowerBound)))

(def default-scale 10)

(defstruct Chart
  :editor
  :chart-panel
  :curve
  :dirty-curve
  :changed-index
  :dragged-entity)