(ns inspector.controller
  (:use util gutil inspector.view)
  (:require global))

(declare inspector)

(defn switch-to-log [] 
  (dosync
   (let [{:keys [log-tab content-panel]} @inspector]
     (alter inspector assoc :selected :log)
     (swing
      (doto (:frame @inspector)
	(.setSize (.getPreferredSize log-tab)))
      (doto content-panel
	(.removeAll)
	(.add log-tab "push, grow")
	(.setSize (.getPreferredSize log-tab))
	(.revalidate)
	(.repaint))))))

(defn switch-to-format [] 
  (dosync 
   (let [{:keys [format-tab content-panel]} @inspector]
     (alter inspector assoc :selected :format)
     (swing 
      (doto content-panel
	(.removeAll)
	(.add format-tab "push, grow")
	(.revalidate)
	(.repaint))))))

(defn switch-to-parameters [] 
  (dosync
   (let [{:keys [parameters-tab content-panel]} @inspector]
     (alter inspector assoc :selected :parameters)
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
     (alter inspector assoc :log-tab tab)
     (when (= :log (:selected @inspector))
       (switch-to-log)))))

(defn update-parameters-tab [type obj]
  (cond 
   (= type :curve)
   (let [tab (create-curve-params-tab obj)]
     (dosync
      (alter inspector assoc :parameters-tab tab)
      (when (= :parameters (:selected @inspector))
	(switch-to-parameters))))))