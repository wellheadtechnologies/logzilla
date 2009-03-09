(import '(org.jfree.chart ChartFactory ChartFrame JFreeChart)
	'(org.jfree.chart.axis NumberAxis)
	'(org.jfree.chart.plot CategoryPlot PlotOrientation)
	'(org.jfree.chart.renderer.category LineAndShapeRenderer)
	'(org.jfree.chart.title TextTitle)
	'(org.jfree.data.category CategoryDataset DefaultCategoryDataset)
	'(org.jfree.ui ApplicationFrame HorizontalAlignment
			     RectangleEdge RefineryUtilities)
	'(org.jfree.data.general DefaultPieDataset))

(load-file "util.clj")

(defn pie-chart-example []
  (let [dataset (new DefaultPieDataset)]
    (doto dataset
      (.setValue "Category 1" 43.2)
      (.setValue "Category 2" 27.9)
      (.setValue "Category 3" 79.5))
    (let [chart (. ChartFactory (createPieChart 
				 "Sample Pie Chart"
				 dataset true true false))
	  frame (new ChartFrame "First" chart)]
      (doto frame
	(.pack)
	(.setVisible true)))))

(defmacro add-values [dataset & tuples]
  `(doto ~dataset
     ~@(for [tuple tuples]
	 `(.addValue ~dataset ~@tuple))))