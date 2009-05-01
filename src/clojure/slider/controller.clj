(ns slider.controller
  (:use util gutil global messages)
  (:import (javax.swing.event ChangeListener)
	   (javax.swing JSlider)))

(declare fire-percentage-change-event)

(defstruct Slider
  :widget
  :notches)

(defn create-depth-slider [slider-notches]
  (let [slider (JSlider. 0 slider-notches 0)]
    (doto slider
      (.setOrientation JSlider/VERTICAL))))

(defn set-percentage [slider new-percentage]
  (let [{:keys [widget notches percentage]} @slider
	value (* (invert new-percentage) notches)]
    (swing
     (ignore :percentage-change slider
	     (.setValue widget value)))))

(defn init-slider-listener [slider]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (swing-event 
		   (let [{:keys [widget notches]} @slider
			 value (.getValue widget)
			 percentage (invert (/ value notches))]
		     (fire :percentage-change slider {:percentage percentage}))))))

(defn init-slider [notches] 
  (let [widget (create-depth-slider notches)
	slider (ref {})
	listener (init-slider-listener slider)
	props (struct-map Slider
		:widget widget
		:notches notches)]
    (dosync (ref-set slider props))
    (doto widget
      (.addChangeListener listener))
    slider))