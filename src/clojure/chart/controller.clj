(ns chart.controller
  (:use chart.model chart.view curves gutil util global lasso)
  (:import (org.jfree.chart ChartMouseListener ChartPanel)
	   (org.jfree.data Range)
	   (org.jfree.chart.renderer.xy XYDifferenceRenderer StandardXYItemRenderer)
	   (java.awt.event MouseAdapter MouseMotionAdapter)
	   (java.awt.geom Point2D Point2D$Double)))

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
		       (when (:dragging-enabled @chart)
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
			   (alter chart assoc :dragged-entity chosen))))))
       (mouseReleased [event]
		      (dosync 
		       (when (:dragging-enabled @chart)
			 (alter chart assoc :dragged-entity nil))))))

(defn set-chart-value [chart curve-index data-index new-value]
  (dosync 
   (let [chart-panel (:chart-panel @chart)
	 curves (:curves @chart)]
     (alter chart assoc-in [:dirty-curves curve-index :data data-index] new-value)
     (alter chart assoc :changes [curve-index data-index])
     (swing
      (let [series (retrieve-series chart-panel curve-index)]
	(.updateByIndex series data-index new-value))))))

(defn chart-drag-listener [chart]
  (proxy [MouseMotionAdapter] []
    (mouseDragged [event]
		  (dosync
		   (when (and (:dragging-enabled @chart) (:dragged-entity @chart))
		     (let [dragged-entity (:dragged-entity @chart)
			   chart-panel (:chart-panel @chart)
			   curve-index (.getSeriesIndex dragged-entity)]
		       (when dragged-entity
			 (swing
			  (let [series (retrieve-series chart-panel curve-index)
				data-index (.getItem dragged-entity)
				new-value (java-2D-to-value chart-panel (.getX event))]
			    (when (not (or (.isNaN new-value) (.isInfinite new-value)))
			      (set-chart-value chart curve-index data-index new-value)))))))))))

(defn custom-chart-panel [chart jfree-chart]
  (let [chart-panel (proxy [ChartPanel] [jfree-chart false false false false false]
		      (zoom [rect]
			    (println "zooming to " rect)
			    (proxy-super zoom rect)))]
    (doto chart-panel
      (.setMouseZoomable false)
      (.addMouseListener (chart-press-listener chart))
      (.addMouseMotionListener (chart-drag-listener chart)))))

;; main controller

(defmulti show-percentage (fn [x y] 
			    (cond 
			      (sequential? x) :charts
			      :else :chart)))

(defmethod show-percentage :charts [charts percentage]
  (doseq [chart charts]
    (show-percentage chart percentage)))

(defmethod show-percentage :chart [chart percentage]
  (dosync 
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
     (when (not= percentage (:percentage @chart))
       (alter chart assoc :percentage percentage))
     (swing 
       (.setRange xaxis (Range. lower upper))
       (.repaint chart-panel)))))

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
  (init-chart [curve] [dirty-curve]))

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

(defn save-chart [chart]
  (dosync
   (let [{:keys [curves dirty-curves]} @chart]
     (doseq [[c d] (tuplize curves dirty-curves)]
       (alter c assoc :data (:data d))))))

