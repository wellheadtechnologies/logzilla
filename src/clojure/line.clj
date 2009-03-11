(import '(org.jfree.chart ChartFactory ChartFrame JFreeChart)
	'(org.jfree.chart.axis NumberAxis)
	'(org.jfree.chart.plot CategoryPlot PlotOrientation)
	'(org.jfree.chart.renderer.category LineAndShapeRenderer)
	'(org.jfree.chart.title TextTitle)
	'(org.jfree.data.category CategoryDataset DefaultCategoryDataset)
	'(org.jfree.ui ApplicationFrame HorizontalAlignment
			     RectangleEdge RefineryUtilities)
	'(org.jfree.data.general DefaultPieDataset)
	'(java.awt Color Dimension Font)
	'(javax.swing JPanel))

(load-file "util.clj")

(defn create-dataset []
  (let [dataset (new DefaultCategoryDataset)]
    (add-values dataset 
		[212 "Classes" "JDK 1.0"]
		[504 "Classes" "JDK 1.1"]
		[1520 "Classes" "SDK 1.2"]
		[1842 "Classes" "SDK 1.3"]
		[2991 "Classes" "SDK 1.4"])
    dataset))

(defn create-chart [dataset]
  (let [chart (. ChartFactory 
		 (createLineChart 
		  "Java Standard Class Library"
		  "Release"
		  "Class Count"
		  dataset
		  (. PlotOrientation VERTICAL)
		  false
		  true
		  false))]
    (.addSubtitle chart (new TextTitle "Number of Classes By Release"))
    (let [source (new TextTitle
		      (str "Source: Java in a Nutshell (4th Edition) "
			   "by David Flanagon (O'Reilly)"))]
      (doto source
	(.setFont (new Font "SansSerif" (. Font PLAIN) 10))
	(.setPosition (. RectangleEdge BOTTOM))
	(.setHorizontalAlignment (. HorizontalAlignment RIGHT)))
      
      (.setBackgroundPaint chart (. Color white))
      
      (doto (.getPlot chart)
	(.setBackgroundPaint (. Color lightGray))
	(.setRangeGridlinePaint (. Color white)))
      
      (.. (.getPlot chart) (getRangeAxis) 
	  (setStandardTickUnits (. NumberAxis (createIntegerTickUnits))))
      
      (doto (.getRenderer (.getPlot chart))
	(.setShapesVisible true)
	(.setDrawOutlines true)
	(.setUseFillPaint true)
	(.setFillPaint (. Color white)))

      chart)))

(defn line-chart-example []
  (let [dataset (create-dataset)
	chart (create-chart dataset)
	frame (new ChartFrame "Line Chart" chart)]
    (doto frame
      (.pack)
      (.setVisible true))))