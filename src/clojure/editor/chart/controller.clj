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
	   scale (:scale @chart)
	   mind (min-depth dirty-curve)
	   maxd (max-depth dirty-curve)
	   range (abs (- maxd mind))
	   unit (/ range scale)
	   extent (+ mind (* percentage range))]
       (alter chart assoc :extent extent)
       (swing 
	 (.setRange xaxis (Range. extent (+ extent unit)))
	 (.repaint chart-panel))))))

(defn init-chart-panel [chart-ref curve]
  (let [jfree-chart (create-chart curve)
	chart-panel (custom-chart-panel chart-ref jfree-chart)]
    (doto chart-panel
      (.setDomainZoomable false)
      (.setMouseZoomable false))))

(defn reset-xaxis [chart-panel min-depth unit-scale]
  (swing 
   (doto (.. chart-panel (getChart) (getPlot) (getDomainAxis))
     (.setAutoRange false)
     (.setRange (Range. min-depth (+ min-depth unit-scale))))))

(defn zoom [old-scales chart]
  (dosync 
   (let [old-scale (get old-scales chart)
	 {:keys [scale chart-panel extent dirty-curve]} @chart
	 mind (min-depth dirty-curve)
	 maxd (max-depth dirty-curve)
	 unit (get-unit-scale mind maxd scale)]
     (when (not= old-scale scale)
       (let [xaxis (.. chart-panel (getChart) (getPlot) (getDomainAxis))]
	 (swing
	  (.setRange xaxis (Range. extent (+ extent unit)))
	  (.repaint chart-panel))))
     (assoc old-scales chart scale))))

(def scale-watcher (agent {}))

(defn change-dragged-plot [chart entity]
  (dosync 
   (alter chart assoc :dragged-entity entity)))

(defn init-chart [editor curve dirty-curve scale] 
  (let [chart (ref nil)
	chart-panel (init-chart-panel chart dirty-curve)
	min-d (min-depth dirty-curve)
	max-d (max-depth dirty-curve)
	props (struct-map Chart 
		:editor editor
		:chart-panel chart-panel
		:curve curve
		:dirty-curve dirty-curve
		:scale scale
		:extent min-d)
	unit-scale (get-unit-scale min-d max-d scale)]
    (dosync (ref-set chart props))
    (add-watcher chart :send scale-watcher zoom)
    (set-validator! chart #(not (>= 0 (:scale %))))
    (reset-xaxis chart-panel min-d unit-scale)
    chart))