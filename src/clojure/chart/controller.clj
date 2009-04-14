(ns chart.controller
  (:use chart.model 
	chart.view
	curves gutil util global
	chart.panel
	lasso)
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

(defn reset-scale 
  ([chart-panel mind unit]
     (swing 
      (.restoreAutoRangeBounds chart-panel)
      (doto (.. chart-panel (getChart) (getPlot) (getDomainAxis))
	(.setAutoRange false)
	(.setRange (Range. mind (+ mind unit))))))
  ([chart]
     (dosync 
      (let [dirty-curve (:dirty-curve @chart)
	    depth-range (get-depth-range dirty-curve)
	    unit (get-unit depth-range default-scale)]
	(reset-scale (:chart-panel @chart) (min-depth dirty-curve) unit)))))

(defn init-chart
  ([curve dirty-curve]
     (let [chart (ref nil)
	   chart-panel (init-chart-panel chart dirty-curve)
	   props (struct-map Chart 
		   :chart-panel chart-panel
		   :curve curve
		   :dirty-curve dirty-curve)]
       (dosync (ref-set chart props))
       (reset-scale chart)
       chart))
  ([curve]
     (init-chart curve (deref-curve @curve))))