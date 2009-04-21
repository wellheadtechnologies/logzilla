(ns editor.slider.controller
  (:use util gutil global
	editor.slider.view editor.slider.model
	messages)
  (:import (javax.swing.event ChangeListener)))

(declare fire-percentage-change-event)

(defn set-percentage [slider percentage]
  (dosync 
   (let [widget (:widget @slider)
	 notches (:notches @slider)
	 value (* percentage notches)]
     (when (and (not= (:percentage @slider) percentage)
		(<= percentage 1) (>= percentage 0))
       (alter slider assoc :percentage percentage)
       (swing
	(ignore :percentage-change slider
		(.setValue widget value)))))))

(defn init-slider-listener [slider]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (swing-event 
		   (dosync 
		    (let [value (.getValue (:widget @slider))
			  notches (:notches @slider)
			  percentage (/ value notches)]
		      (when (not= (:percentage @slider) percentage)
			(alter slider assoc :percentage percentage)
			(fire :percentage-change slider {:percentage percentage}))
		      ))))))

(defn init-slider [notches] 
  (let [widget (create-depth-slider notches)
	slider (ref {})
	listener (init-slider-listener slider)
	props (struct-map Slider
		:widget widget
		:percentage 0
		:notches notches)]
    (dosync (ref-set slider props))
    (doto widget
      (.addChangeListener listener))
    slider))