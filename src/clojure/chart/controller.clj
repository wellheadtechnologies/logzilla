(ns chart.controller
  (:use gutil util global lasso messages)
  (:import (org.jfree.chart ChartMouseListener ChartPanel)
	   (gui ChartUtil ImageUtil CurveIcon)
	   (org.jfree.chart.plot PlotOrientation)
	   (org.jfree.data Range)
	   (org.jfree.ui RectangleEdge)
	   (org.jfree.data.xy XYSeries XYSeriesCollection)
	   (org.jfree.chart.renderer.xy XYDifferenceRenderer StandardXYItemRenderer)
	   (org.jdesktop.swingx.graphics ShadowRenderer)
	   (java.awt.event MouseAdapter MouseMotionAdapter)
	   (java.awt.geom Point2D Point2D$Double)
	   (javax.imageio ImageIO)
	   (java.io File)
	   (java.awt.image BufferedImage)
	   (javax.swing JLabel ImageIcon SwingConstants)
	   (java.awt Toolkit Image Point Cursor Rectangle Color AlphaComposite Font)))

(declare create-chart save-chart update-percentage curve-to-icon)

(defstruct Chart
  :chart-panel
  :curves
  :dirty-curves
  :changes
  :dragged-entity
  :dragging-enabled
  :zooming-enabled
  :showing-points
  :percentage)

(defn min-depth [curve]
  (let [index-data (get-in curve [:index :data])]
    (reduce min index-data)))

(defn max-depth [curve]
  (let [index-data (get-in curve [:index :data])]
    (reduce max index-data)))

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

(defn retrieve-dataset [chart-panel]
  (swing-io!
   (.. chart-panel (getChart) (getPlot) (getDataset))))

(defn retrieve-series [chart-panel curve-index]
  (swing-io! 
    (let [series (. (retrieve-dataset chart-panel) (getSeries))]
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

(defn delta [x y entity]
  (let [bounds (.. entity (getArea) (getBounds))
	bx (. bounds x)
	by (. bounds y)]
    (let [d (sqrt (+ (pow (abs (- x by)) 2)
		      (pow (abs (- y bx)) 2)))]
      d)))

(defn in-range [x y entity]
  (< (delta x y entity) 10))

(defn closest [x y entities]
  (let [len (count entities)]
    (cond 
     (> len 1)
     (let [delta (partial delta x y)]
       (reduce 
	(fn [a b]
	  (if (< (delta a) (delta b))
	    a
	    b))
	entities))

     (= len 1) (first entities))))

(defn chart-press-listener [chart]
  (proxy [MouseAdapter] []
       (mousePressed [event]
		     (swing-io! 
		      (dosync 
		       (when
			(:dragging-enabled @chart)
			(let [chart-panel (:chart-panel @chart)
			      insets (.getInsets chart-panel)
			      x (/ (- (.getX event) (. insets left)) (.getScaleX chart-panel))
			      y (/ (- (.getY event) (. insets top)) (.getScaleY chart-panel))
			      entities (.. chart-panel (getChartRenderingInfo) (getEntityCollection) (getEntities))
			      nearest-entities (if (:showing-points @chart)
						 (filter #(in-range y x %) entities)
						 (filter #(in-range x y %) entities))
			      chosen (if (:showing-points @chart)
				       (closest y x nearest-entities)
				       (closest x y nearest-entities))]
			  (alter chart assoc :dragged-entity chosen)))
		       (when (:panning-enabled @chart)
			(let [chart-panel (:chart-panel @chart)
			      plot (.. chart-panel (getChart) (getPlot))]
			  (alter chart assoc :anchor 
				 [(.getX event) (.getY event) 
				  (.. plot (getRangeAxis) (getRange))
				  (.. plot (getDomainAxis) (getRange))]))))))
       (mouseReleased [event]
		      (swing-io!
		       (dosync 
			(when
			 (:dragging-enabled @chart)
			 (alter chart assoc :dragged-entity nil))
			(when (:panning-enabled @chart)
			  (do
			    (alter chart assoc :anchor nil)))
			(save-chart chart))))))

(defn set-chart-value [chart curve-index data-index new-value]
  (dosync 
   (let [chart-panel (:chart-panel @chart)
	 curves (:curves @chart)]
     (alter chart assoc-in [:dirty-curves curve-index :data data-index] new-value)
     (fire :value-change chart {:curve-index curve-index
				:data-index data-index
				:value new-value})
     (swing
      (let [series (retrieve-series chart-panel curve-index)]
	(.updateByIndex series data-index new-value))))))

(defn center-on [chart x y]
  (swing
   (let [{:keys [chart-panel dirty-curves anchor]} @chart]
     (when anchor
       (let [[anchor-x anchor-y anchor-xrange anchor-yrange] anchor
	     yaxis (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	     ydelta (- (yjava-2D-to-value chart-panel y)
		       (yjava-2D-to-value chart-panel anchor-y))

	     xaxis (.. chart-panel (getChart) (getPlot) (getRangeAxis))
	     xdelta (- (xjava-2D-to-value chart-panel anchor-x)
		       (xjava-2D-to-value chart-panel x))
	     new-xrange (Range. (+ (.getLowerBound anchor-xrange) xdelta)
				(+ (.getUpperBound anchor-xrange) xdelta))
	     new-yrange (Range. (+ (.getLowerBound anchor-yrange) ydelta)
				(+ (.getUpperBound anchor-yrange) ydelta))]
	 (.setRange yaxis new-yrange)
	 (.setRange xaxis new-xrange)
	 (update-percentage chart))))))

(defn chart-drag-listener [chart]
  (proxy [MouseMotionAdapter] []
    (mouseDragged [event]
		  (dosync
		   (cond
		    (and (:dragging-enabled @chart) (:dragged-entity @chart))
		    (let [dragged-entity (:dragged-entity @chart)
			  chart-panel (:chart-panel @chart)
			  curve-index (.getSeriesIndex dragged-entity)]
		      (when dragged-entity
			(swing
			 (let [series (retrieve-series chart-panel curve-index)
			       data-index (.getItem dragged-entity)
			       new-value (xjava-2D-to-value chart-panel (.getX event))]
			   (when (not (or (.isNaN new-value) (.isInfinite new-value)))
			     (set-chart-value chart curve-index data-index new-value))))))
		    
		    (:panning-enabled @chart)
		    (center-on chart (.getX event) (.getY event)))))))

(defn custom-chart-panel [chart jfree-chart]
  (let [chart-panel (proxy [ChartPanel] [jfree-chart false false false false false]
		      (zoom [rect]
			    (proxy-super zoom rect)
			    (update-percentage chart)))]
    (doto chart-panel
      (.setMouseZoomable false)
      (.addMouseListener (chart-press-listener chart))
      (.addMouseMotionListener (chart-drag-listener chart)))))

;; main controller

(defn update-percentage [chart]
  (swing
   (dosync
    (let [{:keys [chart-panel dirty-curves]} @chart
	  xaxis (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	  exemplar (first dirty-curves)
	  mind (min-depth exemplar)
	  depth-range (get-depth-range exemplar)
	  percentage (get-percentage xaxis mind depth-range)]
      (alter chart assoc :percentage percentage)
      (fire :percentage-change chart {:percentage percentage})))))

(defmulti show-percentage (fn [x y] 
			    (cond 
			      (sequential? x) :charts
			      :else :chart)))

(defmethod show-percentage :charts [charts percentage]
  (doseq [chart charts]
    (show-percentage chart percentage)))

(defmethod show-percentage :chart [chart percentage]
  (let [{:keys [chart-panel dirty-curves]} @chart
	xaxis (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	exemplar (first dirty-curves)
	chart-range (get-chart-range xaxis)
	depth-range (get-depth-range exemplar)
	mind (min-depth exemplar)
	scale (get-scale depth-range chart-range)
	unit (get-unit depth-range scale)
	lower (+ mind (* percentage depth-range))
	upper (+ lower unit)]
    (fire :percentage-change chart {:percentage percentage})
    (swing 
     (.setRange xaxis (Range. lower upper))
     (.repaint chart-panel))))

(defmulti init-chart-panel (fn [x y] 
			     (cond 
			       (sequential? y) :curves
			       :else :curve)))

(defmethod init-chart-panel :curves [chart-ref curves]
  (let [jfree-chart (create-chart curves)
	chart-panel (custom-chart-panel chart-ref jfree-chart)]
    chart-panel))

(defmethod init-chart-panel :curve [chart-ref curve]
  (let [jfree-chart (create-chart curve)
	chart-panel (custom-chart-panel chart-ref jfree-chart)]
    chart-panel))

(defn reset [chart]
  (dosync 
   (let [{:keys [chart-panel dirty-curves]} @chart
	 depth-ranges (doall (map get-depth-range dirty-curves))
	 depth-range (first depth-ranges)
	 mind (min-depth (first dirty-curves))
	 unit (get-unit depth-range default-scale)]
     (guard (all-same depth-ranges)
	    "all depth ranges must be equal to scale chart")
     (alter chart assoc :percentage 0)
     (fire :percentage-change chart {:percentage 0})
     (swing 
      (.restoreAutoRangeBounds chart-panel)
      (doto (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	(.setAutoRange false)
	(.setRange (Range. mind (+ mind unit))))))))

(defmulti init-chart (fn [x y] 
		       (cond 
			 (and (sequential? x) (sequential? y)) :multi
			 (and (not (sequential? x)) (not (sequential? y))) :single)))

(defmethod init-chart :multi [curves dirty-curves]
  (let [chart (ref nil)
	chart-panel (init-chart-panel chart dirty-curves)
	props (struct-map Chart 
		:chart-panel chart-panel
		:curves curves
		:dirty-curves dirty-curves)]
    (dosync (ref-set chart props))
    (reset chart)
    chart))

(defmethod init-chart :single [curve dirty-curve]
  (let [chart (ref nil)
	chart-panel (init-chart-panel chart dirty-curve)
	props (struct-map Chart
		:chart-panel chart-panel
		:curves [curve]
		:dirty-curves [dirty-curve])]
    (dosync (ref-set chart props))
    (reset chart)
    chart))

(defn set-scale [chart scale]
  (binding [default-scale scale]
    (reset chart)))

(defn scroll-up
  ([chart delta]
     (dosync 
      (let [{:keys [chart-panel dirty-curves]} @chart
	    xaxis (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	    exemplar (first dirty-curves)
	    depth-range (get-depth-range exemplar)
	    mind (min-depth exemplar)
	    percentage (get-percentage xaxis mind depth-range)]
	(show-percentage chart (+ percentage delta)))))
  ([chart]
     (scroll-up chart 1/100)))

(defn scroll-down 
  ([chart delta]
     (dosync 
      (let [{:keys [chart-panel dirty-curves]} @chart
	    xaxis (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	    exemplar (first dirty-curves)
	    depth-range (get-depth-range exemplar)
	    mind (min-depth exemplar)
	    percentage (get-percentage xaxis mind depth-range)]
	(show-percentage chart (- percentage delta)))))
  ([chart]
     (scroll-down chart 1/100)))

(declare enable-zooming disable-zooming)

(defn enable-dragging [chart]
  (suppress 
    (dosync
     (disable-zooming chart)
     (alter chart assoc :dragging-enabled true))))

(defn disable-dragging [chart]
  (suppress (dosync (alter chart assoc :dragging-enabled false))))

(defn enable-zooming [chart]
  (suppress 
    (dosync 
     (disable-dragging chart)
     (let [chart-panel (:chart-panel @chart)]
       (alter chart assoc :zooming-enabled true)
       (swing 
	 (doto chart-panel
	     (.setMouseZoomable true)
	     (.setFillZoomRectangle false)))))))

(defn disable-zooming [chart]
  (suppress
    (dosync 
     (let [chart-panel (:chart-panel @chart)]
       (alter chart assoc :zooming-enabled false)
       (swing 
	 (doto chart-panel
	   (.setMouseZoomable false)))))))

(defn shade-difference [chart]
  (let [renderer (create-difference-renderer)
	chart-panel (:chart-panel @chart)
	plot (.. chart-panel (getChart) (getPlot))]
    (swing 
      (.setRenderer plot renderer)
      (.repaint chart-panel))))

(defn unshade-difference [chart]
  (let [renderer (create-std-renderer)
	chart-panel (:chart-panel @chart)
	plot (.. chart-panel (getChart) (getPlot))]
    (swing
      (.setRenderer plot renderer)
      (.repaint chart-panel))))

(defn show-points [chart]
  (dosync 
   (when (not (:showing-points @chart))
     (let [chart-panel (:chart-panel @chart)
	   plot (.. chart-panel (getChart) (getPlot))
	   renderer (.getRenderer plot)]
       (alter chart assoc :showing-points true)
       (swing
	 (.setBaseShapesVisible renderer true))))))

(defn hide-points [chart]
  (dosync 
   (when (:showing-points @chart)
     (let [chart-panel (:chart-panel @chart)
	   plot (.. chart-panel (getChart) (getPlot))
	   renderer (.getRenderer plot)]
       (alter chart assoc :showing-points false)
       (swing
	 (.setBaseShapesVisible renderer false))))))

(defn toggle-points [chart]
  (dosync 
   (if (:showing-points @chart)
     (hide-points chart)
     (show-points chart))))

(defn enable-panning [chart]
  (dosync
   (alter chart assoc :panning-enabled true)
   (swing
    (let [chart-panel (:chart-panel @chart)
	  plot (.. chart-panel (getChart) (getPlot))]
      (.setCursor (:chart-panel @chart)
		  (Cursor. Cursor/HAND_CURSOR))))))

(defn disable-panning [chart]
  (dosync 
   (alter chart assoc :panning-enabled false)
   (swing
    (let [chart-panel (:chart-panel @chart)
	  plot (.. chart-panel (getChart) (getPlot))]
      (.setCursor chart-panel (Cursor/getDefaultCursor))))))

(defn toggle-panning [chart]
  (dosync
   (if (:panning-enabled @chart)
     (disable-panning chart)
     (enable-panning chart))))

(defn save-chart [chart]
  (dosync
   (let [{:keys [curves dirty-curves]} @chart]
     (doseq [[c d] (tuplize curves dirty-curves)]
       (alter c assoc :data (:data d))))))

(defn update-curve [chart curve-index curve]
  (swing
   (let [chart-panel (:chart-panel @chart)
	 old-series (retrieve-series chart-panel curve-index)
	 new-series (XYSeries. "Series")
	 dataset (retrieve-dataset chart-panel)
	 index (:index curve)
	 cdata (:data curve)
	 idata (:data index)]
     (.removeSeries dataset old-series)
     (ChartUtil/addToSeries new-series idata cdata)
     (.addSeries dataset new-series)
     (.repaint chart-panel))))