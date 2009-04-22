(ns inspector.controller
  (:use util gutil inspector.view semantics inspector.model)
  (:require global)
  (:import (javax.swing JFrame JPanel JLabel JTextArea
			JTextField JTabbedPane JButton
			JToolBar ButtonGroup JButton 
			JToggleButton Box) 
	   (java.awt Dimension Color)
	   (net.miginfocom.swing MigLayout)))

(declare inspector init-log-tab)

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
  (let [tab (init-log-tab log)]
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

(defn init-log-tab [log]
  (let [panel (JPanel. (MigLayout.))
	semantics (get-semantics @log)
	name (:name semantics)
	location (get-in semantics [:location :data])
	depth-start (get-in semantics [:depth-start :data])
	depth-end (get-in semantics [:depth-end :data])
	company (get-in semantics [:company :data])
	well (get-in semantics [:well :data])
	field (get-in semantics [:field :data])
	province-state (get-in semantics [:province-state :data])
	county (get-in semantics [:county :data])
	country (get-in semantics [:country :data])
	well-id (get-in semantics [:well-id :data])
	width 300
	height (+ 200 (* (count semantics) (.. (JTextField.) (getPreferredSize) (getHeight))))
	update (fn [name val] (update-descriptor log name val))]
    (doto panel
      (.setPreferredSize (Dimension. width height))
      (add-field "Name" name)
      (add-field "Location" location #(update :location %))
      (add-field "Depth Start" depth-start #(update :depth-start %))
      (add-field "Depth End" depth-end #(update :depth-end %))
      (add-field "Company" company #(update :company %))
      (add-field "Well" well #(update :well  %))
      (add-field "Field" field #(update :field %))
      (add-field "Province/State" province-state #(update :province-state %))
      (add-field "County" county #(update :county %))
      (add-field "Country" country #(update :country %))
      (add-field "Well ID" well-id #(update :well-id %)))))
