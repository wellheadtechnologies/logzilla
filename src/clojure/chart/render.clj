(ns chart.render
  (:use util gutil messages)
  (:import (org.jfree.chart ChartMouseListener ChartPanel)
	   (gui ChartUtil ImageUtil CurveIcon)
	   (org.jfree.chart.plot PlotOrientation)
	   (org.jfree.data Range)
	   (org.jfree.ui RectangleEdge)
	   (org.jfree.data.xy XYSeries XYSeriesCollection)
	   (org.jfree.chart.renderer.xy XYDifferenceRenderer StandardXYItemRenderer)
	   (org.jdesktop.swingx.graphics ShadowRenderer)
	   (java.awt.event MouseAdapter MouseMotionAdapter)
	   (java.awt.geom Point2D Point2D$Double Rectangle2D Rectangle2D$Double)
	   (javax.imageio ImageIO)
	   (java.io File FileOutputStream)
	   (java.awt.image BufferedImage)
	   (javax.swing JLabel ImageIcon SwingConstants)
	   (java.awt Toolkit Image Point Cursor Rectangle Color AlphaComposite Font)
	   (com.lowagie.text Document Paragraph PageSize)
	   (com.lowagie.text.pdf PdfWriter)))

(declare create-chart)

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

(defn chart-to-image [chart]
  (let [image (BufferedImage. 400 700 BufferedImage/TYPE_INT_ARGB)
	graphics (.createGraphics image)]
    (.draw chart graphics (Rectangle. 400 700))
    (.dispose graphics)
    image))

(defn curve-to-icon [curve-ref]
  (let [curve @curve-ref
	name (get-in curve [:descriptor :mnemonic])
	chart (create-chart (lasso/deref-curve curve))
	image (chart-to-image chart)]
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
   (let [series (XYSeries. "Series" false)
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
	chart (ChartUtil/createXYLineChart
	       curve-name
	       index-name curve-name
	       dataset PlotOrientation/HORIZONTAL
	       false)
	plot (.getPlot chart)]
    (.setRenderer plot (create-std-renderer))
    (.setBackgroundPaint plot Color/white)
    chart))

(defmethod create-chart :multi [curves]
  (guard (all-same (map :index curves))
	 "indices of curves for multi-chart must be equal")
  (let [dataset (create-dataset curves)
	chart (ChartUtil/createXYLineChart
	       "Chart" 
	       "x" "y"
	       dataset PlotOrientation/HORIZONTAL
	       false)
	plot (.getPlot chart)]
    (.setRenderer plot (create-std-renderer))
    (.setBackgroundPaint plot Color/white)
    chart))

(defn export-chart-to-pdf [chart width height file-path]
  (let [chart-panel (:chart-panel @chart)
	jfree-chart (.getChart chart-panel)
	page-size (com.lowagie.text.Rectangle. width height)
	document (Document. PageSize/A4)
	writer (PdfWriter/getInstance document (FileOutputStream. file-path))
	image (chart-to-image jfree-chart)]
    (doto document
      (.open))
    (.add document (Paragraph. "Hello World"))
    (let [itext-image (com.lowagie.text.Image/getInstance writer image (float 1.0))]
      (.setAbsolutePosition itext-image 15 15)
      (.add document itext-image))
    (.close document)))
