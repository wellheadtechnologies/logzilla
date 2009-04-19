(ns inspector.controller
  (:use util gutil inspector.view)
  (:require global))

(declare inspector)

(defn switch-to-log [] 
  (dosync
   (let [{:keys [log-tab content-panel]} @inspector]
     (swing
      (doto content-panel
	(.removeAll)
	(.add log-tab "push, grow")
	(.revalidate)
	(.repaint))))))

(defn switch-to-format [] 
  (dosync 
   (let [{:keys [format-tab content-panel]} @inspector]
     (swing 
      (doto content-panel
	(.removeAll)
	(.add format-tab "push, grow")
	(.revalidate)
	(.repaint))))))

(defn switch-to-parameters [] 
  (dosync
   (let [{:keys [parameters-tab content-panel]} @inspector]
     (swing
      (doto content-panel
	(.removeAll)
	(.add parameters-tab "push, grow")
	(.revalidate)
	(.repaint))))))

(defn init-inspector []
  (create-inspector-window switch-to-log switch-to-format switch-to-parameters))

(def inspector (ref (init-inspector)))

(defn open-inspector []
  (swing (.setVisible (:frame @inspector) true)))

(defn update-log-tab [log]
  (let [tab (create-log-tab log)]
    (dosync
     (alter inspector assoc :log-tab tab))))