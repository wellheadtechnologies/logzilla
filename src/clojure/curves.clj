(ns curves 
  (:require lasso)
  (:use util chart.view)
  (:import (org.jfree.chart ChartFactory)
	   (gui ImageUtil CurveIcon)
	   (org.jfree.chart.plot PlotOrientation)
	   (org.jfree.data.xy XYSeries XYSeriesCollection)
	   (org.jfree.ui RectangleEdge)
	   (java.awt Rectangle Color AlphaComposite Font)
	   (java.awt.image BufferedImage)
	   (javax.swing JLabel ImageIcon SwingConstants)
	   (org.jdesktop.swingx.graphics ShadowRenderer)))

(def icon-width 64)
(def icon-height 64)
(def band-width icon-width)
(def band-height (int (/ icon-height 5)))
(def band-y (- (half icon-height)
	       (half band-height)))
(def string-length 10)
(defn string-y [font string font-render-context]
  (let [bounds (.getStringBounds font string 0 (.length string) font-render-context)
	font-height (.getHeight bounds)]
    (int (dec (+ band-y (half band-height) (half font-height))))))

(defn string-x [font string font-render-context]
  (let [bounds (.getStringBounds font string 0 (.length string) font-render-context)
	font-width (.getWidth bounds)]
    (int (- (half band-width) (half font-width)))))

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
	name (get-in curve [:descriptor :mnemonic])
	chart (create-chart (lasso/deref-curve curve))
	image (BufferedImage. 400 700 BufferedImage/TYPE_INT_ARGB)
	graphics (.createGraphics image)]
    (.draw chart graphics (Rectangle. 400 700))
    (.dispose graphics)
    (let [final-image (render-shadow (fast-scale image icon-width icon-height))
	  graphics (.createGraphics final-image)
	  font (Font. "Helvetica" Font/BOLD 10)
	  truncated-name (if (> (.length name) string-length)
			   (.substring name 0 string-length)
			   name)]
      (doto graphics
	(.setColor Color/black)
	(.fillRect 0 band-y band-width band-height)
	(.setColor Color/white)
	(.setFont font)
	(.drawString truncated-name
		     (string-x font truncated-name (.getFontRenderContext graphics))
		     (string-y font truncated-name (.getFontRenderContext graphics)))
	(.dispose))
      (CurveIcon. curve-ref
		   (ImageIcon. final-image)))))

(defn min-depth [curve]
  (let [index-data (get-in curve [:index :data])]
    (reduce min index-data)))

(defn max-depth [curve]
  (let [index-data (get-in curve [:index :data])]
    (reduce max index-data)))