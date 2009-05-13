(ns slider.controller
  (:use util gutil global messages)
  (:import (javax.swing.event ChangeListener)
	   (javax.swing JSlider)))

(declare fire-percentage-change)

(defstruct Slider
  :widget
  :notches)

(defn create-depth-slider [slider-notches]
  (let [slider (JSlider. 0 slider-notches 0)]
    (doto slider
      (.setOrientation JSlider/VERTICAL))))

(defmulti set-percentage (fn [x y]
			   (cond 
			    (number? y) :percentage
			    (= (class y) clojure.lang.PersistentArrayMap) :event)))

(defmethod set-percentage :event [slider event]
  (set-percentage slider (:percentage event)))

(defmethod set-percentage :percentage [slider new-percentage]
  (let [{:keys [widget notches percentage]} @slider
	value (* (invert new-percentage) notches)]
    (swing-agent
     (ignore :percentage-change slider
	     (.setValue widget value)))))

(defn init-slider-listener [slider]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (swing-mutator
		   (let [{:keys [widget notches]} @slider
			 value (.getValue widget)
			 percentage (invert (/ value notches))]
		     (fire-percentage-change slider percentage))))))

(defn init-slider [notches] 
  (let [widget (create-depth-slider notches)
	slider (ref {})
	listener (init-slider-listener slider)
	props (struct-map Slider
		:widget widget
		:notches notches)]
    (dosync (ref-set slider props))
    (add-receiver :percentage-change slider #(set-percentage slider %))
    (doto widget
      (.addChangeListener listener))
    slider))

(defn fire-percentage-change [slider percentage]
  (fire :percentage-change slider {:percentage percentage :source slider}))
