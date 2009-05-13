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
	  chart (init-chart curve (deref-curve @curve))
	  frame (JFrame.)]
      (doto frame
	(.add (:chart-panel @chart))
	(.setSize (Dimension. 300 700))
	(.setVisible true))
      chart)))

(defn show-multi-chart []
  (binding [chart.model/default-scale 1]
    (let [lasfile (load-lasfile "las_files/test.las")
	  curve1 (first (:curves @lasfile))
	  curve2 (second (:curves @lasfile))
	  chart (init-chart [curve1 curve2] [(deref-curve @curve1) (deref-curve @curve2)])
	  frame (JFrame.)]
      (doto frame
	(.add (:chart-panel @chart))
	(.setSize (Dimension. 300 700))
	(.setVisible true))
      chart)))
