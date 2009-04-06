(ns editor.slider.controller
  (:use util gutil global storage
	editor.slider.view)
  (:import (javax.swing.event ChangeListener)))

;slider = slider-id

(defn reset-notches [] nil)

(defn init-slider-listener [editor slider]
  (proxy [ChangeListener] []
    (stateChanged [event]
		  (swing 
		   (let [value (.getValue slider)]
		     (change-in slider [:value] value))))))

(defn get-instance-properties [editor slider notches]
  (instance-properties 
   [:editor editor]
   [:slider slider]
   [:value 0, :filter #(/ % notches)]
   [:notches notches]))

(defn init-slider [editor notches] 
  (let [slider (create-depth-slider notches)
	listener (init-slider-listener editor slider)
	props (get-instance-properties editor slider notches)
	sprops (store-properties slider props)]
    (doto slider
      (.addChangeListener listener))))

