(ns gui.ceditor
  (:use util las))

(import '(java.awt BorderLayout Color)
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

(defn- create-dataset [curve]
  (let [series (new XYSeries "Series")
	ds (new XYSeriesCollection)
	index (:index curve)]
    (doseq [[x y] (tuplize (:data index) (:data curve))]
      (.add series x y))
    (.addSeries ds series)
    ds))

(defn- create-chart [dataset curve]
  (let [chart (ChartFactory/createXYLineChart 
	       (str (:mnemonic curve) " Chart")
	       (:mnemonic (:index curve))
	       (:mnemonic curve) 
	       dataset PlotOrientation/HORIZONTAL
	       false false false)
	plot (.getPlot chart)
	renderer (.getRenderer plot)]
    (.setBasePaint renderer Color/blue)
    (.setSeriesPaint renderer 0 Color/blue)
    (.setBackgroundPaint plot Color/white)
    chart))


(defn- create-table [curve]
  (let [index (:index curve)
	table (new JTable)
	model (new DefaultTableModel)]
    (doto model
	(.addColumn (:mnemonic index) (into-array Object (:data index)))
	(.addColumn (:mnemonic curve) (into-array Object (:data curve))))
    (.setModel table model)
    table))

(defn curve-to-image [curve]
  (let [dataset (create-dataset curve)
	chart (create-chart dataset curve)
	image (.createBufferedImage chart 400 700)]
    (.getScaledInstance image 64 64 Image/SCALE_SMOOTH)))

(defn open-curve-editor [curve]
  (let [index (:index curve)
	min-depth (cmin index)
	max-depth (cmax index)
	depth-slider (create-depth-slider min-depth max-depth)
	dataset (create-dataset curve)
	chart (create-chart dataset curve)
	table (create-table curve)
	table-pane (new JScrollPane table)
	chart-panel (new ChartPanel chart)
	main-panel (new JPanel (new MigLayout))
	frame (new JFrame "Editor")
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