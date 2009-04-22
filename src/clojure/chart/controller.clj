(ns chart.controller
  (:use chart.model chart.view curves gutil util global lasso messages)
  (:import (org.jfree.chart ChartMouseListener ChartPanel)
	   (org.jfree.data Range)
	   (org.jfree.chart.renderer.xy XYDifferenceRenderer StandardXYItemRenderer)
	   (java.awt.event MouseAdapter MouseMotionAdapter)
	   (java.awt.geom Point2D Point2D$Double)
	   (javax.imageio ImageIO)
	   (java.io File)
	   (java.awt Toolkit Image Point Cursor)))

(declare update-percentage save-chart)

;; panel 

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

(defmethod show-percentage :chart [chart new-percentage]
  (dosync 
   (let [{:keys [chart-panel dirty-curves percentage]} @chart
	 xaxis (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	 exemplar (first dirty-curves)
	 chart-range (get-chart-range xaxis)
	 depth-range (get-depth-range exemplar)
	 mind (min-depth exemplar)
	 scale (get-scale depth-range chart-range)
	 unit (get-unit depth-range scale)
	 lower (+ mind (* new-percentage depth-range))
	 upper (+ lower unit)]
     (when (not= new-percentage (:percentage @chart))
       (alter chart assoc :percentage new-percentage)
       (fire :percentage-change chart {:percentage percentage})
       (swing 
	(.setRange xaxis (Range. lower upper))
	(.repaint chart-panel))))))

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