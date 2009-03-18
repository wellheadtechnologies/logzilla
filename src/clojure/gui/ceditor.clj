(ns gui.ceditor
  (:use util gui.util))

(import '(java.awt BorderLayout Color)
	'(gui ChartUtil CustomChartPanel CustomJTable)
	'(javax.swing BorderFactory JPanel JSlider JWindow JFrame
		      JTable JScrollPane)
	'(javax.swing.table DefaultTableModel)
	'(javax.swing.event ChangeListener)
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
				 DatasetUtilities)
	'(org.jfree.data.time Minute RegularTimePeriod
			      TimeSeries TimeSeriesCollection)
	'(org.jfree.data.xy AbstractXYDataset XYDataset XYSeries
			    XYSeriesCollection)
	'(org.jfree.ui ApplicationFrame RectangleInsets 
		       RefineryUtilities)
	'(net.miginfocom.swing MigLayout)
	'(java.awt Dimension Image))

(def dragged-item (agent nil))
(def slider-n 100)
(def scale-n 10)

(defn get-scale [range]
  (let [diff (- (.getUpperBound range) (.getLowerBound range))
	scale (/ diff scale-n)]
    scale))

(defn scale-value [scale value]
  (let [ratio (/ slider-n scale-n)]
    (* (/ value ratio) scale)))

(defn- create-slider-listener [depth-slider xaxis min-depth table]
  (let [scale (get-scale (.getRange xaxis))]
    (proxy [ChangeListener] []
      (stateChanged [event]
		    (let [value (.getValue depth-slider)
			  scaled (scale-value scale value)]
		      (.setRange xaxis 
				 (+ scaled min-depth)
				 (+ scaled min-depth (* scale 1)))
		      (.showAtPercentage table (- 1 (/ value slider-n)))
		      )))))

(defn- create-depth-slider [min-depth max-depth]
  (let [slider (new JSlider 0 slider-n 0)]
    (.setOrientation slider JSlider/VERTICAL)
    slider))

(defn- create-table [curve]
  (let [index (.getIndex curve)
	table (new CustomJTable)
	model (new DefaultTableModel)]
    (doto model
	(.addColumn (.getMnemonic index) (into-array Object (reverse (.getLasData index))))
	(.addColumn (.getMnemonic curve) (into-array Object (reverse (.getLasData curve)))))
    (.setModel table model)
    table))

(defn open-curve-editor [curve]
  (let [index (.getIndex curve)
	depth-data (.getLasData index)
	min-depth (reduce min depth-data)
	max-depth (reduce max depth-data)
	depth-slider (create-depth-slider min-depth max-depth)
	chart (ChartUtil/createChart curve)
	table (create-table curve)
	table-pane (new JScrollPane table)
	chart-panel (new CustomChartPanel curve chart)
	main-panel (new JPanel (new MigLayout))
	frame (new JFrame (str (.getMnemonic curve) " Editor"))
	plot (.getPlot chart)
	x-axis (.getDomainAxis plot)
	scale (get-scale x-axis)]

    (.addChangeListener 
     depth-slider 
     (create-slider-listener depth-slider x-axis min-depth table))

    (doto chart-panel
      (.setDomainZoomable false)
      (.setMouseZoomable false))

    (.addChartMouseListener 
     chart-panel
     (proxy [ChartMouseListener] []
       (chartMouseClicked 
	[e] 
	(send dragged-item 
	      (fn [entity]
		(if entity
		  nil ;unset dragged-item
		  (.getEntity e)))))
       (chartMouseMoved 
	[e] 
	(send dragged-item 
	      (fn [entity]
		(when entity
		  (let [mouse-event (.getTrigger e)
			dataset (.getDataset entity)
			series (first (.getSeries dataset))
			index (.getItem entity)
			item (.getDataItem series index)
			bounds (.. entity (getArea) (getBounds))
			new-value (.java2DToValue chart-panel (.getX mouse-event))]
		    (swing 
		     (when (not (or (.isNaN new-value) (.isInfinite new-value)))
		       (.updateByIndex series index new-value)
		       (.repaint chart-panel)))))
		entity)))))
    
    (doto x-axis
      (.setAutoRange false)
      (.setRange (new Range min-depth (+ min-depth (* scale 1)))))
      
    (doto main-panel
      (.setPreferredSize (new Dimension 700 700))
      (.add depth-slider "pushy, growy")
      (.add table-pane "pushy, growy")
      (.add chart-panel "pushy, growy"))
      
    (doto frame
      (.add main-panel)
      (.pack)
      (.setVisible true))

    frame))