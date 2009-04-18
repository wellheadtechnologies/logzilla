(ns editor.slider.controller
  (:use util gutil global
	editor.slider.view editor.slider.model)
  (:import (javax.swing.event ChangeListener)))

(declare fire-percentage-change-event)

(defn set-percentage [slider percentage]
  (dosync 
   (let [widget (:widget @slider)
	 notches (:notches @slider)]
     (when (not= (:percentage @slider)
		 percentage)
       (alter slider assoc :percentage percentage)
       (swing
	 (.setValue widget (* percentage notches)))))))

(defn init-slider-listener [slider]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (swing 
		   (dosync 
		    (let [value (.getValue (:widget @slider))
			  notches (:notches @slider)
			  percentage (/ value notches)]
		      (alter slider assoc :percentage percentage)
		      (fire-percentage-change-event slider percentage)))))))

(defn init-slider [notches] 
  (let [widget (create-depth-slider notches)
	slider (ref {})
	listener (init-slider-listener slider)
	props (struct-map Slider
		:widget widget
		:percentage 0
		:notches notches
		:percentage-change-listeners [])]
    (dosync (ref-set slider props))
    (doto widget
      (.addChangeListener listener))
    slider))

(defn fire-percentage-change-event [slider percentage]
  (let [event {:percentage percentage}
	listeners (:percentage-change-listeners @slider)]
    (fire-event listeners event)))

(defn add-percentage-change-listener [slider listener]
  (add-listener :percentage-change-listeners slider listener))

(defn remove-percentage-change-listener [slider listener]
  (remove-listener :percentage-change-listeners slider listener))