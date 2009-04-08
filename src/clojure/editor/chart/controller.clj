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
	   notches (:scale-notches @chart)
	   mind (min-depth dirty-curve)
	   maxd (max-depth dirty-curve)
	   range (abs (- maxd mind))
	   unit (/ range notches)
	   extent (+ mind (* percentage range))]
       (swing 
	 (.setRange xaxis (Range. extent (+ extent unit)))
	 (.repaint chart-panel))))))

(defn init-chart-panel [chart-ref curve]
  (let [jfree-chart (create-chart curve)
	chart-panel (custom-chart-panel chart-ref jfree-chart)]
    (doto chart-panel
      (.setDomainZoomable false)
      (.setMouseZoomable false))))

(defn reset-xaxis [chart-panel min-depth scale]
  (swing 
   (doto (.. chart-panel (getChart) (getPlot) (getDomainAxis))
     (.setAutoRange false)
     (.setRange (Range. min-depth (+ min-depth scale))))))

(defn change-dragged-plot [chart entity]
  (dosync 
   (alter chart assoc :dragged-entity entity)))

(defn init-chart [editor curve dirty-curve scale-notches] 
  (let [chart (ref nil)
	chart-panel (init-chart-panel chart dirty-curve)
	props (struct-map Chart 
		:editor editor
		:chart-panel chart-panel
		:curve curve
		:dirty-curve dirty-curve
		:scale-notches scale-notches)
	min-d (min-depth dirty-curve)
	max-d (max-depth dirty-curve)
	scale (get-scale min-d max-d scale-notches)]
    (dosync (ref-set chart props))
    (reset-xaxis chart-panel min-d scale)
    chart))