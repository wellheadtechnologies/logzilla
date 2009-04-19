(ns chart.model
  (:use gutil util curves)
  (:import (org.jfree.ui RectangleEdge)))

(defn xjava-2D-to-value [chart-panel x]
  (swing-io! 
   (let [chart (.getChart chart-panel)
	 xaxis (.. chart (getPlot) (getRangeAxis))
	 value (.java2DToValue xaxis x (.getScreenDataArea chart-panel) RectangleEdge/TOP)]
     value)))

(defn yjava-2D-to-value [chart-panel y]
  (swing-io! 
   (let [chart (.getChart chart-panel)
	 yaxis (.. chart (getPlot) (getDomainAxis))
	 value (.java2DToValue yaxis y (.getScreenDataArea chart-panel) RectangleEdge/TOP)]
     value)))

(defn retrieve-series [chart-panel curve-index]
  (swing-io! 
    (let [series (.. chart-panel (getChart) (getPlot) (getDataset) (getSeries))]
      (nth series curve-index))))

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

(defn get-percentage [xaxis mind depth-range]
  (let [lower (.. xaxis (getRange) (getLowerBound))]
    (/ (- lower mind) depth-range)))

(def default-scale 10)

(defn dirty-curve [chart]
  (only (:dirty-curves @chart)))

(defstruct Chart
  :chart-panel
  :curves
  :dirty-curves
  :changes
  :dragged-entity
  :dragging-enabled
  :zooming-enabled
  :showing-points
  :percentage
  :value-change-listeners
  :percentage-change-listeners)