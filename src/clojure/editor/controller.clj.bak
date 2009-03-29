(ns curves.controller
  (:use curves.model util gutil global))

(def dragged-entities (agent {})) ;chart-panel -> chart-entity (panel to plot)
(def chart-columns (agent {})) ;chart-panel -> table-column
(def curve-charts (agent {})) ;curve -> chart
(def saved-curves (agent {})) ;lasfile -> curves

(defn not-dragging-anything [] (= 0 (count @dragged-entities)))

(defn configure-x-axes [x-axes max-depth min-depth]
  (doseq [x-axis x-axes]
    (let [scale (get-scale max-depth min-depth)]
      (doto x-axis 
	(.setAutoRange false)
	(.setRange (new Range min-depth (+ min-depth scale)))))))

(defn- add-chart-panels [main-panel table chart-panels]
  (let [model (.getModel table)]
    (doseq [i (range 0 (count chart-panels))]
      (let [cp (nth chart-panels i)]
	(send chart-columns assoc cp (inc i))
	(send curve-charts assoc (.getCurve cp) (.getChart cp))
	(doto cp
	  (.setDomainZoomable false)
	  (.setMouseZoomable false))
	
	(.addChartMouseListener cp (create-chart-mouse-listener cp table))
	(.addTableModelListener model (create-table-model-listener table cp))
	(.add main-panel cp "pushy, growy")))))

(defn- save-curves [lasfile curves]
  (doseq [c curves] (.info logger "Saving Curve {}" c))
  (send saved-curves assoc lasfile curves))

(defn- change-dragged-plot [chart-panel chart-event]
  (send dragged-entities
	(fn [entities]
	  (let [entity (get entities chart-panel)]
	    (if entity
	      (dissoc entities chart-panel)
	      (assoc entities chart-panel (.getEntity chart-event)))))))

(defn- drag-plot [chart-panel table chart-event]
  (send dragged-entities
	(fn [entities]
	  (let [entity (get entities chart-panel)]
	    (when entity
	      (let [mouse-event (.getTrigger chart-event)
		    series (first (.. entity (getDataset) (getSeries)))
		    index (.getItem entity)
		    new-value (.java2DToValue chart-panel (.getX mouse-event))]
		(swing 
		 (when (not (or (.isNaN new-value) (.isInfinite new-value)))
		   (.updateByIndex series index new-value)
		   (sync-table-with-chart table chart-panel index)
		   (.repaint chart-panel))))))
	  entities)))

(defn- create-chart-mouse-listener [chart-panel table]
  (proxy [ChartMouseListener] []
    (chartMouseClicked [e] (change-dragged-plot chart-panel e))
    (chartMouseMoved [e] (drag-plot chart-panel table e))))

(defn- dirty-curves-for [curves]
  (doall
   (for [curve curves]
     (let [chart (get @curve-charts curve)]
       (let [series (first (.. chart (getPlot) (getDataset) (getSeries)))
	     points (.getItems series)]
	 (new ImmutableCurve
	      (.getDescriptor curve)
	      (.getIndex curve)
	      (map #(.getY %) points)))))))

(defn- sync-chart-with-table [table chart-panel row]
  (swing 
   (let [model (.getModel table)
	 chart (.getChart chart-panel)
	 series (first (.. chart (getPlot) (getDataset) (getSeries)))
	 column (get @chart-columns chart-panel)]
     (let [index (row-to-index row table)]
       (.updateByIndex series index 
		       (let [value (.getValueAt model row column)]
			 (if (string? value)
			   (Double/valueOf value)
			   (double value))))
       (.repaint chart-panel)))))

(defn- sync-table-with-chart [table chart-panel index]
  (let [model (.getModel table)
	chart (.getChart chart-panel)
	series (first (.. chart (getPlot) (getDataset) (getSeries)))
	row (index-to-row index table)
	column (get @chart-columns chart-panel)
	item (.getDataItem series index)
	new-value (.getY item)]
    (swing
     (.setValueAt (.getModel table) new-value row column)
     (.repaint table))))

;; listeners

(defn- create-table-model-listener [table chart-panel]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (when (not-dragging-anything)
		    (sync-chart-with-table table chart-panel (.getFirstRow e))))))

(defn- create-slider-listener [depth-slider xaxes max-depth min-depth table]
  (let [scale (get-scale max-depth min-depth)]
    (proxy [ChangeListener] []
      (stateChanged [event]
		    (let [value (.getValue depth-slider)
			  scaled (scale-value scale value)
			  lower (+ scaled min-depth)
			  upper (+ lower scale)]
		      (doseq [xaxis xaxes]
			(.setRange xaxis 
				   (if (> lower (- max-depth scale))
				     (- max-depth scale)
				     lower)
				   (if (> upper max-depth)
				     max-depth
				     upper)))
		      (.showAtPercentage table (- 1 (/ value slider-n)))
		      )))))

