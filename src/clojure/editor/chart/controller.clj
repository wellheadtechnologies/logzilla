(ns editor.chart.controller
  (:use editor.chart.model 
	editor.chart.view
	curves gutil util global)
  (:import (gui CustomChartPanel)
	   (org.jfree.chart ChartMouseListener)
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

(defn init-chart-panel [curve]
  (let [chart (create-chart curve)
	chart-panel (new CustomChartPanel chart)]
    (doto chart-panel
      (.setDomainZoomable false)
      (.setMouseZoomable false))))

(defn reset-xaxis [chart-panel min-depth scale]
  (swing 
   (doto (.. chart-panel (getChart) (getPlot) (getDomainAxis))
     (.setAutoRange false)
     (.setRange (Range. min-depth (+ min-depth scale))))))

;(defn change-dragged-plot [chart-id chart-event]
;  (change-in chart-id [:dragged-entity] (.getEntity chart-event)))
;
;(defn drag-plot [chart-id chart-event]
;  (dosync 
;   (let [dragged-entity (lookup-in chart-id :dragged-entity)
;	 chart-panel (lookup-in chart-id :chart-panel)]
;     (when dragged-entity
;       (swing
;	(let [mouse-event (.getTrigger chart-event)
;	      series (retrieve-series chart-panel)
;	      index (.getItem dragged-entity)
;	      new-value (java-2D-to-value chart-panel (.getX mouse-event))]
;	  (when (not (or (.isNaN new-value) (.isInfinite new-value)))
;	    (.updateByIndex series index new-value)
;	    (.repaint chart-panel)
;	    (dosync 
;	     (change-in chart-id [:dirty-curve :data index] new-value)
;	     (change-in chart-id [:changed-index] index)))))))))
;
;(defn init-chart-mouse-listener [chart curve]
;  (proxy [ChartMouseListener] []
;    (chartMouseClicked [e] (change-dragged-plot chart curve e))
;    (chartMouseMoved [e] (drag-plot chart-id curve e))))

(defn init-chart [editor curve dirty-curve scale-notches] 
  (let [chart-panel (init-chart-panel dirty-curve)
	chart (ref (struct-map Chart 
		     :editor editor
		     :chart-panel chart-panel
		     :curve curve
		     :dirty-curve dirty-curve
		     :scale-notches scale-notches))
	min-d (min-depth dirty-curve)
	max-d (max-depth dirty-curve)
	scale (get-scale min-d max-d scale-notches)]
    (reset-xaxis chart-panel min-d scale)
    chart))