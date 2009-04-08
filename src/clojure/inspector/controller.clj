(ns inspector.controller
  (:use util gutil inspector.view)
  (:require global))

(defn init-inspector []
  (create-inspector-window 200 400))

(def inspector (init-inspector))

(defn open-inspector []
  (swing (.setVisible (:frame inspector) true)))

(defn switch-to-curves-tab [curves]
  (swing 
   (doto (:panel inspector)
     (.remove 0)
     (.addTab "Curve" (create-curves-tab curves)))))

(defn switch-inspector-tab [to object]
  (cond 
   (= to :curves) (when (< 0 (count object)) (switch-to-curves-tab object))))

(dosync 
 (ref-set global/switch-inspector-tab-method switch-inspector-tab))