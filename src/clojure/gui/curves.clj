(ns gui.curves
  (:use util gui.util core.las))

(import '(java.awt BorderLayout Color)
	'(gui ChartUtil CustomChartPanel CustomJTable)
	'(javax.swing BorderFactory JPanel JSlider JWindow JFrame
		      JTable JScrollPane)
	'(javax.swing.table DefaultTableModel)
	'(javax.swing.event ChangeListener TableModelListener)
	'(javax.swing.border Border)
	'(javax.swing.event ChangeEvent ChangeListener)
	'(org.jfree.chart ChartFactory ChartPanel JFreeChart)
	'(org.jfree.chart.axis DateAxis)
	'(org.jfree.chart ChartMouseListener)
	'(org.jfree.chart.plot PlotOrientation XYPlot)
	'(org.jfree.chart.renderer.xy XYItemRenderer)
	'(org.jfree.data Range)
	'(org.jfree.data.general DatasetChangeEvent
				 DatasetChangeListener
				 DatasetUtilities
				 SeriesChangeListener)
	'(org.jfree.data.time Minute RegularTimePeriod
			      TimeSeries TimeSeriesCollection)
	'(org.jfree.data.xy AbstractXYDataset XYDataset XYSeries
			    XYSeriesCollection)
	'(org.jfree.ui ApplicationFrame RectangleInsets 
		       RefineryUtilities)
	'(net.miginfocom.swing MigLayout)
	'(java.awt Dimension Image))

(def dragged-entities (agent {})) ;chart-panel -> chart-entity (panel to plot)
(def slider-n 100)
(def scale-n 10)

(def chart-columns (agent {})) ;chart-panel -> table-column

(defn get-scale [range]
  (let [diff (- (.getUpperBound range) (.getLowerBound range))
	scale (/ diff scale-n)]
    scale))

(defn scale-value [scale value]
  (let [ratio (/ slider-n scale-n)]
    (* (/ value ratio) scale)))

(defn- create-slider-listener [depth-slider xaxis max-depth min-depth table]
  (let [scale (get-scale (.getRange xaxis))]
    (proxy [ChangeListener] []
      (stateChanged [event]
		    (let [value (.getValue depth-slider)
			  scaled (scale-value scale value)
			  lower (+ scaled min-depth)
			  upper (+ lower scale)]
		      (.setRange xaxis 
				 (if (> lower (- max-depth scale))
				   (- max-depth scale)
				   lower)
				 (if (> upper max-depth)
				   max-depth
				   upper))
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

    (doseq [[curve curve-data] (tuplize curves curve-datas)]
      (.addColumn model (.getMnemonic curve) (into-array Object curve-data)))

    (.setModel table model)
    table))

(defn- index-to-row [index table]
  (- (dec (.getRowCount table)) index))

(defn- row-to-index [row table]
  (- (dec (.getRowCount table)) row))

(defn- sync-chart-with-table [table chart-panel row]
  (when (get @dragged-entities chart-panel)
    (swing 
     (let [model (.getModel table)
	   chart (.getChart chart-panel)
	   series (first (.. chart (getPlot) (getDataset) (getSeries)))
	   column (get @chart-columns chart-panel)]
       (let [index (row-to-index row table)]
	 (.updateByIndex series index (Double/valueOf (.getValueAt model row column)))
	 (.repaint chart-panel))))))

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
		  (sync-chart-with-table table chart-panel (.getFirstRow e)))))

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

(defn open-curve-editor [curves]
  (let [index (largest-index curves)
	depth-data (.getLasData index)
	min-depth (reduce min depth-data)
	max-depth (reduce max depth-data)
	depth-slider (create-depth-slider min-depth max-depth)
	charts (map #(ChartUtil/createChart %) curves)
	table (create-table index curves)
	table-pane (new JScrollPane table)
	chart-panels (map (fn [[curve chart]]
			    (new CustomChartPanel curve chart))
			  (tuplize curves charts))
	main-panel (new JPanel (new MigLayout))
	frame (new JFrame (str "Curves Editor"))
	plots (map #(.getPlot %) charts)
	x-axes (map #(.getDomainAxis %) plots)]
    
    (doseq [x-axis x-axes]
      (let [scale (get-scale x-axis)]
	(.addChangeListener depth-slider 
			    (create-slider-listener depth-slider
						    x-axis max-depth
						    min-depth table))
	
	(doto x-axis
	  (.setAutoRange false)
	  (.setRange (new Range min-depth (+ min-depth (* scale 1)))))
	))

    (doto main-panel
      (.add depth-slider "pushy, growy")
      (.add table-pane "pushy, growy"))

    (doseq [i (range 0 (count chart-panels))]
      (let [chart-panel (nth chart-panels i)]
	(send chart-columns assoc chart-panel (inc i))

	(doto chart-panel
	  (.setDomainZoomable false)
	  (.setMouseZoomable false))
	
	(.addChartMouseListener chart-panel
				(create-chart-mouse-listener chart-panel table))
	
	(.addTableModelListener (.getModel table)
				(create-table-model-listener table chart-panel))
	
	(.add main-panel chart-panel "pushy, growy")))

    (let [width (* 600 (count chart-panels))
	  height 700]
      (.setPreferredSize main-panel (new Dimension width height)))

    (println (.getPreferredSize main-panel))
    
    (doto table
      (.showAtPercentage 1))
    
    (doto frame
      (.add main-panel)
      (.pack)
      (.setVisible true))
    
    frame))

  