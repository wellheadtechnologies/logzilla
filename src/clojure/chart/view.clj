(ns chart.view
  (:require lasso)
  (:use util)
  (:import (org.jfree.chart ChartFactory)
	   (gui ImageUtil CurveLabel)
	   (org.jfree.chart.plot PlotOrientation)
	   (org.jfree.data.xy XYSeries XYSeriesCollection)
	   (org.jfree.ui RectangleEdge)
	   (java.awt Rectangle Color AlphaComposite)
	   (java.awt.image BufferedImage)
	   (javax.swing JLabel ImageIcon SwingConstants)
	   (org.jdesktop.swingx.graphics ShadowRenderer)
	   (org.jfree.chart.renderer.xy XYDifferenceRenderer StandardXYItemRenderer)))

(def default-line-color Color/blue)
(def default-background-color Color/white)

(defn create-std-renderer []
  (let [renderer (StandardXYItemRenderer.)]
    (doto renderer
      (.setBasePaint Color/blue)
      (.setSeriesPaint 0 Color/blue)
      (.setSeriesPaint 1 Color/red))))

(defn create-difference-renderer []
  (let [renderer (XYDifferenceRenderer.)]
    renderer))

(defmulti create-dataset (fn [x] 
			   (cond 
			     (sequential? x) :multi
			     :else :single)))

(defmethod create-dataset :single [curve]
  (io!
    (let [series (XYSeries. "Series")
	  dataset (XYSeriesCollection.)
	  index (:index curve)
	  cdata (:data curve)
	  idata (:data index)]
      (doseq [[x y] (tuplize idata cdata)]
	(.add series x y))
      (.addSeries dataset series)
      dataset)))

(defmethod create-dataset :multi [curves]
  (io!
    (let [dataset (XYSeriesCollection.)]
      (doseq [curve curves]
	(let [series (XYSeries. "Series")
	      index (:index curve)
	      cdata (:data curve)
	      idata (:data index)]
	  (doseq [[x y] (tuplize idata cdata)]
	    (.add series x y))
	  (.addSeries dataset series)))
      dataset)))

(defmulti create-chart (fn [x] 
			 (cond 
			   (sequential? x) :multi
			   :else :single)))

(defmethod create-chart :single [curve]
  (let [dataset (create-dataset curve)
	curve-name (get-in curve [:descriptor :mnemonic])
	index-name (get-in curve [:index :descriptor :mnemonic])
	chart (ChartFactory/createXYLineChart
	       (str curve-name " Chart")
	       index-name curve-name
	       dataset PlotOrientation/HORIZONTAL
	       false false false)
	plot (.getPlot chart)]
    (.setRenderer plot (create-std-renderer))
    (.setBackgroundPaint plot Color/white)
    chart))

(defmethod create-chart :multi [curves]
  (guard (all-same (map :index curves))
	 "indices of curves for multi-chart must be equal")
  (let [dataset (create-dataset curves)
	chart (ChartFactory/createXYLineChart
	       "Chart" 
	       "x" "y"
	       dataset PlotOrientation/HORIZONTAL
	       false false false)
	plot (.getPlot chart)]
    (.setRenderer plot (create-std-renderer))
    (.setBackgroundPaint plot Color/white)
    chart))