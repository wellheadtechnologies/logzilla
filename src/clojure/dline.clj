(ns dline
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
	'(org.jfree.data.xy AbstractXYDataset XYDataset)
	'(org.jfree.ui ApplicationFrame RectangleInsets 
		       RefineryUtilities)
	'(demo TranslatingXYDataset))

(def create-chart)
(def create-dataset)
(def slider-listener)
(def slider (new JSlider -200 200 0))
(def dataset)

(defn demo-panel []
  (binding [dataset (create-dataset "Random 1" 100.0 (new Minute) 100)]    
    (let [border-layout (new BorderLayout)
	  chart (create-chart)
	  chart-panel (new ChartPanel chart)
	  frame (new ApplicationFrame "App")]
      (doto chart-panel
	(.setPreferredSize (new java.awt.Dimension 600 270))
	(.setDomainZoomable true)
	(.setRangeZoomable true))
      (let [border (. BorderFactory 
		      (createCompoundBorder 
		       (. BorderFactory (createEmptyBorder 4 4 4 4))
		       (. BorderFactory (createEtchedBorder))))]
	(.setBorder chart-panel border)
	(.add frame chart-panel)
	(let [dashboard (new JPanel (new BorderLayout))]
	  (.setBorder dashboard (. BorderFactory (createEmptyBorder 0 4 4 4)))
	  (doto slider
	    (.setPaintLabels true)
	    (.setMajorTickSpacing 50)
	    (.setPaintTicks true)
	    (.addChangeListener (slider-listener dataset)))
	  (.add dashboard slider)
	  (.add frame dashboard (. BorderLayout SOUTH))))
      (doto frame
	(.pack)
	(.setVisible true)))))
				 
(defn create-chart []
  (let [chart1 (. ChartFactory (createTimeSeriesChart 
				"Translate Demo 1" "Time of Day"
				"Value"	dataset true true false))
	plot (.getXYPlot chart1)
	renderer (.getRenderer plot)
	axis (.getDomainAxis plot)
	range (. DatasetUtilities (findDomainBounds dataset))]
    (.setRange axis range)
    (.setBackgroundPaint chart1 (. Color white))
    (.setPaint renderer (. Color black))
    (doto plot
      (.setOrientation (. PlotOrientation VERTICAL))
      (.setBackgroundPaint (. Color lightGray))
      (.setDomainGridlinePaint (. Color white))
      (.setRangeGridlinePaint (. Color white))
      (.setAxisOffset (new RectangleInsets 5.0 5.0 5.0 5.0))
      (.setDomainCrosshairVisible true)
      (.setDomainCrosshairLockedOnData false)
      (.setRangeCrosshairVisible false))
    chart1))

(defn create-dataset [name base start count]
  (let [series (new TimeSeries name (.getClass start))
	vfn #(* % (inc (/ (- (. Math (random)) 0.495) 10.0)))
	values (iterate vfn base)
	periods (iterate #(.next %) start)]
    (doseq [[period value] (take count (tuplize periods values))]
      (.add series period value))
    (let [tsc (new TimeSeriesCollection)
	  ds (new TranslatingXYDataset tsc)]
      (.addSeries tsc series)
      ds)))
      
(defn slider-listener [dataset] 
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (let [value (.getValue slider)]
		    (.setTranslate dataset (* value 60 1000.0))))))