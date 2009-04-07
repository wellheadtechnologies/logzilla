(ns editor.slider.controller
  (:use util gutil global
	editor.slider.view editor.slider.model)
  (:import (javax.swing.event ChangeListener)))

(defn reset-notches [] nil)

(defn init-slider-listener [slider]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (swing 
		   (dosync 
		    (let [value (.getValue (:widget @slider))
			  notches (:notches @slider)]
		      (alter slider assoc :value (/ value notches))))))))

(defn init-slider [notches] 
  (let [widget (create-depth-slider notches)
	slider (ref {})
	listener (init-slider-listener slider)
	props (struct-map Slider
		:widget widget
		:value 0
		:notches notches)]
    (dosync (ref-set slider props))
    (doto widget
      (.addChangeListener listener))
    slider))

