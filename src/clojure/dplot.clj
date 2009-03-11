(ns dplot
  (:use util))
(import '(java.awt BorderLayout Color)
	'(javax.swing BorderFactory JPanel JSlider)
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
	'(demo TranslatingXYDataset))

(def slider (new JSlider -50 50 0))

(defn create-dataset []
  (let [series (new XYSeries "Series")
	ds (new XYSeriesCollection)]
    (doseq [i (range 0 100)]
      (.add series i i))
    (.addSeries ds series)
    ds))
    

(defn create-chart []
  (. ChartFactory 
     (createXYLineChart 
      "Demo Chart" "x" "y"
      (create-dataset)
      (. PlotOrientation VERTICAL)
      false false false)))

(defn slider-listener [domain-axis]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (let [value (.getValue slider)
			range (.getRange domain-axis)]
		    (println "value = " value)
		    (.setRange domain-axis (new Range value (+ value 50)))))))

(defn demo-panel []
  (let [chart (create-chart)
	chart-panel (new ChartPanel chart)
	main-panel (new JPanel)
	frame (new ApplicationFrame "Demo")
	plot (.getPlot chart)
	domain-axis (.getDomainAxis plot)]
    (.addChangeListener slider (slider-listener domain-axis))

    (doto domain-axis
      (.setAutoRange false)
      (.setRange (new Range 0 50)))

    (doto main-panel
      (.add chart-panel)
      (.add slider))
    
    (doto frame
      (.add main-panel)
      (.pack)
      (.setVisible true))))