(ns curves 
  (:require lasso)
  (:use util chart.view)
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