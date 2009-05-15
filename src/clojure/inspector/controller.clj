(ns inspector.controller
  (:use util gutil semantics)
  (:require global registry)
  (:import (javax.swing JFrame JPanel JLabel JTextArea
			JTextField JTabbedPane JButton
			JToolBar ButtonGroup JButton 
			JToggleButton Box) 
	   (java.awt Dimension Color)
	   (net.miginfocom.swing MigLayout)))

(declare inspector init-log-tab)

(defstruct Inspector 
  :frame 
  :tab-bar
  :content-panel
  :log-tab
  :format-tab
  :parameters-tab
  :selected)

(defn add-field 
  ([panel name value]
     (add-field panel name value (fn [f] nil)))
  ([panel name value behavior]
      (let [label (JLabel. name)
	    field (JTextField. value)]
	(doto label
	  (.putClientProperty "JComponent.sizeVariant" "small"))
	(doto field 
	  (.putClientProperty "JComponent.sizeVariant" "small")
	  (on-action (behavior (.getText field))))
	(doto panel
	  (.add label)
	  (.add field "pushx, growx, wrap")))))

(defn add-text-area [panel name value]
  (let [label (JLabel. name)
	area (JTextArea.)]
    (doto area
      (.setLineWrap true)
      (.setText value))
    (doto panel
      (.add label)
      (.add area "push, grow"))))

(defn create-curve-params-tab [curve]
  (let [panel (JPanel. (MigLayout.))]
    (dosync
     (doto panel
       (add-field "Name" (get-in @curve [:descriptor :mnemonic]))
       (add-field "Unit" (get-in @curve [:descriptor :unit]))
       (add-text-area "Description" (get-in @curve [:descriptor :description]))))))

(defn tab-button [name action]
  (let [button (JToggleButton. name)]
    (on-action button (action))
    (doto button
      (.putClientProperty "JComponent.sizeVariant" "small")
      (.putClientProperty "JButton.buttonType" "segmentedGradient")
      (.setPreferredSize (Dimension. 200 10)))))

(defn init-tab-bar [log-action format-action parameter-action]
  (let [panel (JPanel. (MigLayout. "ins 0, gapx 0"))
	button-group (ButtonGroup.)
	log-button (tab-button "Log" log-action)
	format-button (tab-button "Format" format-action)
	parameter-button (tab-button "Params" parameter-action)]
    (doto button-group
      (.add log-button)
      (.add format-button)
      (.add parameter-button))
    (doto panel
      (.add (doto log-button (.putClientProperty "JButton.segmentPosition" "first") (.setSelected true)))
      (.add (doto format-button (.putClientProperty "JButton.segmentPosition" "middle")))
      (.add (doto parameter-button (.putClientProperty "JButton.segmentPosition" "last"))))))

(defn create-inspector-window [log-action format-action parameter-action]
  (let [frame (registry/acquire-registered-frame)
	panel (JPanel. (MigLayout. "ins 0"))
	content-panel (JPanel. (MigLayout. "ins 0"))
	tab-bar (init-tab-bar log-action format-action parameter-action)]
    (.. frame (getRootPane) (putClientProperty "Window.style" "small"))
    (doto panel 
      (.add tab-bar "wrap")
      (.add content-panel "push, grow"))
    (doto frame
      (.setTitle "Inspector")
      (.add panel)
      (.setSize (Dimension. 300 400)))
    (struct-map Inspector
      :frame frame
      :tab-bar tab-bar
      :content-panel content-panel
      :log-tab (JPanel.)
      :format-tab (JPanel.)
      :parameters-tab (JPanel.)
      :selected :log)))

(defn switch-to-log [] 
  (dosync
   (let [{:keys [log-tab content-panel]} @inspector]
     (alter inspector assoc :selected :log)
     (swing-agent
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
     (swing-agent
      (doto content-panel
	(.removeAll)
	(.add format-tab "push, grow")
	(.revalidate)
	(.repaint))))))

(defn switch-to-parameters [] 
  (dosync
   (let [{:keys [parameters-tab content-panel]} @inspector]
     (alter inspector assoc :selected :parameters)
     (swing-agent
      (doto content-panel
	(.removeAll)
	(.add parameters-tab "push, grow")
	(.revalidate)
	(.repaint))))))

(defn init-inspector []
  (create-inspector-window switch-to-log switch-to-format switch-to-parameters))

(def inspector (ref (init-inspector)))

(defn open-inspector []
  (swing-agent (.setVisible (:frame @inspector) true)))

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
