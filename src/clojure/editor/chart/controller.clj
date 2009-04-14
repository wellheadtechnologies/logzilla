(ns editor.chart.controller
  (:use editor.chart.model 
	editor.chart.view
	curves gutil util global
	editor.chart.panel)
  (:import (org.jfree.chart ChartMouseListener)
	   (org.jfree.data Range)))

(defn show-percentage [charts percentage]
  (doseq [chart charts]
    (dosync 
     (let [chart-panel (:chart-panel @chart)
	   xaxis (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	   dirty-curve (:dirty-curve @chart)
	   chart-range (get-chart-range xaxis)
	   depth-range (get-depth-range dirty-curve)
	   mind (min-depth dirty-curve)
	   scale (get-scale depth-range chart-range)
	   unit (get-unit depth-range scale)
	   lower (+ mind (* percentage depth-range))
	   upper (+ lower unit)]
       (swing 
	(.setRange xaxis (Range. lower upper))
	(.repaint chart-panel))))))

(defn init-chart-panel [chart-ref curve]
  (let [jfree-chart (create-chart curve)
	chart-panel (custom-chart-panel chart-ref jfree-chart)]
    chart-panel))

(defn reset-scale [chart-panel mind unit]
  (swing 
   (.restoreAutoRangeBounds chart-panel)
   (doto (.. chart-panel (getChart) (getPlot) (getDomainAxis))
     (.setAutoRange false)
     (.setRange (Range. mind (+ mind unit))))))

(defn change-dragged-plot [chart entity]
  (dosync 
   (alter chart assoc :dragged-entity entity)))

(defn init-chart [editor curve dirty-curve] 
  (let [chart (ref nil)
	chart-panel (init-chart-panel chart dirty-curve)
	props (struct-map Chart 
		:editor editor
		:chart-panel chart-panel
		:curve curve
		:dirty-curve dirty-curve)
	depth-range (get-depth-range dirty-curve)
	unit (get-unit depth-range default-scale)]
    (dosync (ref-set chart props))
    (reset-scale chart-panel (min-depth dirty-curve) unit)
    chart))