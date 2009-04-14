(ns curves 
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
	   (org.jdesktop.swingx.graphics ShadowRenderer)))

(defn stack-images [top bottom]
  (let [graphics (.createGraphics bottom)]
    (doto graphics
      (.setComposite (AlphaComposite/getInstance AlphaComposite/SRC_OVER 1.0))
      (.drawImage top 0 0 nil)
      (.dispose))
    bottom))

(defn render-shadow [image]
  (let [shadow-renderer (new ShadowRenderer)
	shadow (.createShadow shadow-renderer image)]
    (stack-images image shadow)))

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
	plot (.getPlot chart)
	renderer (.getRenderer plot)]
    (doto renderer
      (.setBasePaint Color/blue)
      (.setSeriesPaint 0 Color/blue))
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
	plot (.getPlot chart)
	renderer (.getRenderer plot)]
    (doto renderer
      (.setBasePaint Color/blue)
      (.setSeriesPaint 0 Color/blue))
    (.setBackgroundPaint plot Color/white)
    chart))

(defn fast-scale [image x y] (ImageUtil/fastScale image x y))

(defn curve-to-icon [curve-ref]
  (let [curve @curve-ref
	chart (create-chart (lasso/deref-curve curve))
	image (BufferedImage. 400 700 BufferedImage/TYPE_INT_ARGB)
	graphics (.createGraphics image)]
    (.draw chart graphics (Rectangle. 0 0 400 700) nil nil)
    (.dispose graphics)
    (let [final-image (render-shadow (fast-scale image 64 64))
	  icon (ImageIcon. final-image)
	  name (get-in curve [:descriptor :mnemonic])]
      (CurveLabel. curve-ref name icon SwingConstants/LEFT))))

(defn min-depth [curve]
  (let [index-data (get-in curve [:index :data])]
    (reduce min index-data)))

(defn max-depth [curve]
  (let [index-data (get-in curve [:index :data])]
    (reduce max index-data)))