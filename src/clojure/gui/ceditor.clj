(ns gui.ceditor
  (:use util las))

(import '(java.awt BorderLayout Color)
	'(javax.swing BorderFactory JPanel JSlider JWindow JFrame)
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
	'(java.awt Dimension))

(defn create-depth-slider [min max]
  (let [slider (new JSlider min max min)]
    (.setOrientation slider JSlider/VERTICAL)
    slider))

(defn- create-dataset [curve index]
  (let [series (new XYSeries "Series")
	ds (new XYSeriesCollection)]
    (doseq [[x y] (tuplize (:data index) (:data curve))]
      (.add series x y))
    (.addSeries ds series)
    ds))

(defn- create-chart [dataset]
  (ChartFactory/createXYLineChart 
   "Curve Chart" "x" "y"
   dataset PlotOrientation/HORIZONTAL
   false false false))

(defn- create-slider-listener [depth-slider x-axis]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (let [value (.getValue depth-slider)
			range (.getRange x-axis)]
		    (.setRange x-axis value (+ value 100))))))

(defn open-curve-editor [curve index]
  (let [min-depth (Math/round (cmin index))
	max-depth (Math/round (cmax index))]
    (let [dataset (create-dataset curve index)
	  chart (create-chart dataset)
	  chart-panel (new ChartPanel chart)
	  main-panel (new JPanel (new MigLayout))
	  frame (new JFrame "Editor")
	  plot (.getPlot chart)
	  x-axis (.getDomainAxis plot)
	  depth-slider (create-depth-slider min-depth max-depth)]

      (doto plot 
	(.setBackgroundPaint Color/white))

      (.addChangeListener depth-slider (create-slider-listener depth-slider x-axis))
      (doto x-axis
      (.setAutoRange false)
      (.setRange (new Range min-depth (+ min-depth 100))))

      (doto main-panel
	(.setPreferredSize (new Dimension 400 700))
	(.add depth-slider "pushy, growy")
	(.add chart-panel "pushy, growy"))    

      (doto frame
	(.add main-panel)
	(.pack)
	(.setVisible true))
      frame)))
