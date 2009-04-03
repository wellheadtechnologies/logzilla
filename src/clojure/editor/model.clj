(ns editor.model
  (:require lasso)
  (:import (org.jfree.ui RectangleEdge)))

(defstruct FrameWidgets
  :table
  :depth-slider
  :main-panel)

(defstruct FrameData
  :lasfile-id
  :index
  :min-depth
  :max-depth
  :scale-notches
  :slider-notches
  :xaxes
  :width 
  :height)

(defstruct Chart
  :dirty-curve
  :chart-panel
  :table-column
  :dragged-entity)

(def frame-charts (ref {})) ;; frame -> {curve-id -> Chart}
(def frame-data (ref {})) ;; frame -> FrameData
(def frame-widgets (ref {})) ;; frame -> FrameWidgets

;pure
(defn get-scale [data]
  (let [{:keys [max-depth min-depth scale-notches]} data
	diff (- max-depth min-depth)
	scale (/ diff scale-notches)]
    scale))

;pure
(defn scale-value [data value]
  (let [scale (get-scale data)
	{:keys [slider-notches scale-notches]} data
	ratio (/ slider-notches scale-notches)]
    (* (/ value ratio) scale)))

;impure
(defn java-2D-to-value [chart-panel x]
  (let [chart (.getChart chart-panel)
	xaxis (.. chart (getPlot) (getRangeAxis))
	value (.java2DToValue xaxis x (.getScreenDataArea chart-panel) RectangleEdge/TOP)]
    value))

;impure
(defn retrieve-series [chart-panel]
  (first (.. chart-panel (getChart) (getPlot) (getDataset) (getSeries))))

;impure
(defn index-to-row [index table]
  (- (dec (.getRowCount table)) index))

;impure
(defn row-to-index [row table]
  (- (dec (.getRowCount table)) row))

