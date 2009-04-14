(ns tests.tchart
  (:use util gutil global lasso
	chart.controller curves)
  (:require chart.model)
  (:import (javax.swing JFrame)
	   (java.awt Dimension)))

(defn show-chart []
  (binding [chart.model/default-scale 1]
    (let [lasfile (load-lasfile "las_files/test.las")
	  curve (first (:curves @lasfile))
	  chart (init-chart curve)
	  frame (JFrame.)]
      (doto frame
	(.add (:chart-panel @chart))
	(.setSize (Dimension. 300 700))
	(.setVisible true)))))

