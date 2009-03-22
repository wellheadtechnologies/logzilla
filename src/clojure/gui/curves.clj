(ns gui.curves
  (:use util gui.util core.las gui.widgets)
  (:import (java.awt BorderLayout Color)
	   (org.slf4j Logger LoggerFactory)
	   (core DefaultCurve)
	   (gui ChartUtil CustomChartPanel CustomJTable)
	   (javax.swing BorderFactory JPanel JSlider JWindow JFrame
			JTable JScrollPane JButton)
	   (javax.swing.table DefaultTableModel)
	   (javax.swing.event ChangeListener TableModelListener)
	   (javax.swing.border Border)
	   (javax.swing.event ChangeEvent ChangeListener)
	   (org.jfree.chart ChartFactory ChartPanel JFreeChart)
	   (org.jfree.chart.axis DateAxis)
	   (org.jfree.chart ChartMouseListener)
	   (org.jfree.chart.plot PlotOrientation XYPlot)
	   (org.jfree.chart.renderer.xy XYItemRenderer)
	   (org.jfree.data Range)
	   (org.jfree.data.general DatasetChangeEvent
				   DatasetChangeListener
				   DatasetUtilities
				   SeriesChangeListener)
	   (org.jfree.data.time Minute RegularTimePeriod
				TimeSeries TimeSeriesCollection)
	   (org.jfree.data.xy AbstractXYDataset XYDataset XYSeries
			      XYSeriesCollection)
	   (org.jfree.ui ApplicationFrame RectangleInsets 
			 RefineryUtilities)
	   (net.miginfocom.swing MigLayout)
	   (java.awt Dimension Image)))

(def open-curve-editor)

(def dragged-entities (agent {})) ;chart-panel -> chart-entity (panel to plot)
(def chart-columns (agent {})) ;chart-panel -> table-column
(def curve-charts (agent {})) ;curve -> chart
(def saved-curves (agent {})) ;lasfile -> curves

(def slider-n 100)
(def scale-n 10)

(def logger (LoggerFactory/getLogger "gui.curves"))

(defn get-scale [max-depth min-depth]
  (let [diff (- max-depth min-depth)
	scale (/ diff scale-n)]
    scale))

(defn scale-value [scale value]
  (let [ratio (/ slider-n scale-n)]
    (* (/ value ratio) scale)))

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

(defn- create-depth-slider [min-depth max-depth]
  (let [slider (new JSlider 0 slider-n 0)]
    (.setOrientation slider JSlider/VERTICAL)
    slider))

(defn- create-table [index curves]
  (let [table (new CustomJTable)
	model (new DefaultTableModel)
	index-data (large-to-small (.getLasData index))
	do-reverse (not= (first index-data) (first (.getLasData index)))
	curve-datas (if do-reverse
		      (map #(reverse (.getLasData %)) curves)
		      (map #(.getLasData %) curves))]
    
    (.addColumn model (.getMnemonic index) (into-array Object index-data))

    (doseq [curve curves]
      (adjust-curve index curve))

    (doseq [[curve curve-data] (tuplize curves curve-datas)]
      (let [curve-index-data (large-to-small (.getLasData (.getIndex curve)))
	    srate (isample-rate index)
	    begin-padding (repeat (start-offset index-data curve-index-data srate) 0)
	    after-padding (repeat (end-offset index-data curve-index-data srate) 0)]
	(.addColumn model (.getMnemonic curve) 
		    (into-array Object (concat 
					begin-padding
					curve-data
					after-padding)))
	))
    (.setModel table model)
    table))

(defn- index-to-row [index table]
  (- (dec (.getRowCount table)) index))

(defn- row-to-index [row table]
  (- (dec (.getRowCount table)) row))

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

(defn- create-table-model-listener [table chart-panel]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (when (= 0 (count @dragged-entities))
		    (sync-chart-with-table table chart-panel (.getFirstRow e))))))

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
	 (new DefaultCurve
	      (.getDescriptor curve)
	      (.getIndex curve)
	      (map #(.getY %) points)))))))

(defn- save-curves [lasfile curves]
  (doseq [c curves] (.info logger "Saving Curve {}" c))
  (send saved-curves assoc lasfile curves))

(defn- merge-button [lasfile index curves]
  (button "Merge" 
    (fn [e]
      (open-curve-editor lasfile [(merge-curves index curves)]))))

(defn- save-button [lasfile curves]
  (button "Save"
    (fn [e]
      (save-curves lasfile (dirty-curves-for curves)))))

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

(defn configure-xaxes [x-axes max-depth min-depth]
  (doseq [x-axis x-axes]
    (let [scale (get-scale max-depth min-depth)]
      (doto x-axis 
	(.setAutoRange false)
	(.setRange (new Range min-depth (+ min-depth (* scale 1))))))))


(defn open-curve-editor [lasfile curves]
  (guard (all-samef (map sample-rate curves))
	 "Sample rates must be the same")

  (let [index (largest-index curves)
	acurves (map #(adjust-curve index %) curves)
	depth-data (.getLasData index)
	min-depth (reduce min depth-data)
	max-depth (reduce max depth-data)
	depth-slider (create-depth-slider min-depth max-depth)
	charts (map #(ChartUtil/createChart %) acurves)
	table (create-table index acurves)
	table-pane (new JScrollPane table)
	chart-panels (map (fn [[curve chart]]
			    (new CustomChartPanel curve chart))
			  (tuplize acurves charts))
	frame (new JFrame (str "Curves Editor"))
	plots (map #(.getPlot %) charts)
	x-axes (map #(.getDomainAxis %) plots)
	mergeb (merge-button lasfile index acurves)
	saveb (save-button lasfile acurves)

	tool-panel (panel 
		    [depth-slider "pushy, growy"]
		    [table-pane "pushy, growy, wrap"]
		    [saveb ""]
		    [mergeb ""])

	main-panel (panelS 
		    (* 600 (count chart-panels)) 700
		    [tool-panel "pushy, growy"])
	]

    (configure-xaxes x-axes max-depth min-depth)

    (add-chart-panels main-panel table chart-panels)

    (.addChangeListener depth-slider 
			(create-slider-listener depth-slider
						x-axes max-depth
						min-depth table))    
    (doto table
      (.showAtPercentage 1))
    
    (doto frame
      (.add main-panel)
      (.pack)
      (.setVisible true))
    
    frame))