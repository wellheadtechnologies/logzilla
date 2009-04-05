(ns editor.slider.view
  (:import (javax.swing JSlider)))

(defn create-depth-slider [slider-notches]
  (let [slider (new JSlider 0 slider-notches 0)]
    (doto slider
      (.setOrientation JSlider/VERTICAL))))