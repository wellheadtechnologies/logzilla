(ns slider.view
  (:import (javax.swing JSlider)))

(defn create-depth-slider [slider-notches]
  (let [slider (JSlider. 0 slider-notches 0)]
    (doto slider
      (.setOrientation JSlider/VERTICAL))))