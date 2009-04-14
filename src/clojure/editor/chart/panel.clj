(ns editor.chart.panel
  (:use util gutil global editor.chart.model)
  (:import (org.jfree.chart ChartPanel)
	   (java.awt.event MouseAdapter MouseMotionAdapter)
	   (java.awt.geom Point2D Point2D$Double)))

(defn sqrt [x] (Math/sqrt x))
(defn pow [x y]
  (Math/pow x y))

(defn delta [x y entity]
  (let [bounds (.. entity (getArea) (getBounds))
	bx (. bounds x)
	by (. bounds y)]
    (sqrt (+ (pow (abs (- x by)) 2)
	     (pow (abs (- y bx)) 2)))))

(defn in-range [x y entity]
  (< (delta x y entity) 10))

(defn closest [x y entities]
  (let [len (count entities)]
    (cond 
     (> len 1)
     (let [delta (partial delta x y)]
       (reduce 
	(fn [a b]
	  (if (< (delta a) (delta b))
	    a
	    b))
	entities))
     
     (= len 1) (first entities))))

(defn chart-press-listener [chart]
  (proxy [MouseAdapter] []
       (mousePressed [event]
		     (swing-io! 
		      (dosync 
		       (let [chart-panel (:chart-panel @chart)
			     insets (.getInsets chart-panel)
			     x (/ (- (.getX event) (. insets left)) (.getScaleX chart-panel))
			     y (/ (- (.getY event) (. insets top)) (.getScaleY chart-panel))
			     entities (.. chart-panel (getChartRenderingInfo) (getEntityCollection) (getEntities))
			     nearest-entities (filter #(in-range x y %) entities)
			     chosen (closest x y nearest-entities)]
			 (alter chart assoc :dragged-entity chosen)))))
       (mouseReleased [event]
		      (dosync (alter chart assoc :dragged-entity nil)))))

(defn set-chart-value [chart index new-value]
  (dosync 
   (let [chart-panel (:chart-panel @chart)]
     (alter chart assoc-in [:dirty-curve :data index] new-value)
     (alter chart assoc :changed-index index)
     (swing
      (let [series (retrieve-series chart-panel)]
	(.updateByIndex series index new-value))))))

(defn chart-drag-listener [chart]
  (proxy [MouseMotionAdapter] []
    (mouseDragged [event]
		  (dosync 
		   (let [dragged-entity (:dragged-entity @chart)
			 chart-panel (:chart-panel @chart)]
		     (when dragged-entity
		       (swing
			(let [series (retrieve-series chart-panel)
			      index (.getItem dragged-entity)
			      new-value (java-2D-to-value chart-panel (.getX event))]
			  (when (not (or (.isNaN new-value) (.isInfinite new-value)))
			    (set-chart-value chart index new-value))))))))))

(defn custom-chart-panel [chart jfree-chart]
  (let [chart-panel (proxy [ChartPanel] [ jfree-chart false false false false false]
		      (zoom [rect] 
			    (proxy-super zoom rect))
		      )]
    (doto chart-panel
      (.setMouseZoomable true)
      (.setFillZoomRectangle false)
      (.addMouseListener (chart-press-listener chart))
      (.addMouseMotionListener (chart-drag-listener chart)))))