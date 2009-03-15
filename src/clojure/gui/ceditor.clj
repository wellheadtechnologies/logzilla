(ns gui.ceditor
  (:use util))

(import '(java.awt BorderLayout Color)
	'(gui ChartUtil)
	'(javax.swing BorderFactory JPanel JSlider JWindow JFrame
		      JTable JScrollPane)
	'(javax.swing.table DefaultTableModel)
	'(javax.swing.event ChangeListener)
	'(javax.swing.border Border)
	'(javax.swing.event ChangeEvent ChangeListener)
	'(org.jfree.chart ChartFactory ChartPanel JFreeChart)
	'(org.jfree.chart.axis DateAxis)
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

(defn- create-slider-listener [depth-slider x-axis]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (let [value (.getValue depth-slider)
			range (.getRange x-axis)]
		    (.setRange x-axis value (+ value 100))))))

(defn- create-depth-slider [min-depth max-depth]
  (let [slider (new JSlider min-depth max-depth min-depth)]
    (.setOrientation slider JSlider/VERTICAL)
    slider))

(defn- create-table [curve]
  (let [index (.getIndex curve)
	table (new JTable)
	model (new DefaultTableModel)]
    (doto model
	(.addColumn (.getMnemonic index) (into-array Object (.getLasData index)))
	(.addColumn (.getMnemonic curve) (into-array Object (.getLasData curve))))
    (.setModel table model)
    table))

(defn open-curve-editor [curve]
  (let [index (.getIndex curve)
	depth-data (.getLasData index)
	min-depth (first depth-data)
	max-depth (last depth-data)
	depth-slider (create-depth-slider min-depth max-depth)
	chart (ChartUtil/createChart curve)
	table (create-table curve)
	table-pane (new JScrollPane table)
	chart-panel (new ChartPanel chart)
	main-panel (new JPanel (new MigLayout))
	frame (new JFrame (str (.getMnemonic curve) " Editor"))
	plot (.getPlot chart)
	x-axis (.getDomainAxis plot)]

    (.addChangeListener depth-slider (create-slider-listener depth-slider x-axis))
      
    (doto x-axis
      (.setAutoRange false)
      (.setRange (new Range min-depth (+ min-depth 100))))
      
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