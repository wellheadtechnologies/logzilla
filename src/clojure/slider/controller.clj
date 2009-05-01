(ns slider.controller
  (:use util gutil global messages)
  (:import (javax.swing.event ChangeListener)
	   (javax.swing JSlider)))

(declare fire-percentage-change-event)

(defstruct Slider
  :widget
  :percentage
  :notches)

(defn create-depth-slider [slider-notches]
  (let [slider (JSlider. 0 slider-notches 0)]
    (doto slider
      (.setOrientation JSlider/VERTICAL))))

(defn set-percentage [slider new-percentage]
  (dosync 
   (let [{:keys [widget notches percentage]} @slider
	 value (* (invert new-percentage) notches)]
     (when (and (not= percentage new-percentage)
		(<= new-percentage 1) (>= new-percentage 0))
       (alter slider assoc :percentage new-percentage)
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
			  percentage (invert (/ value notches))]
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
		:percentage 1
		:notches notches)]
    (dosync (ref-set slider props))
    (doto widget
      (.addChangeListener listener))
    slider))