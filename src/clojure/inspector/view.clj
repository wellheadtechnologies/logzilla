(ns inspector.view
  (:use global gutil semantics inspector.model)
  (:import (javax.swing JFrame JPanel JLabel JTextArea
			JTextField JTabbedPane JButton
			JToolBar ButtonGroup JButton 
			JToggleButton Box) 
	   (java.awt Dimension Color)
	   (net.miginfocom.swing MigLayout)))

(defn add-field [panel name value]
  (let [label (JLabel. name)
	field (JTextField. value)]
    (doto label
      (.putClientProperty "JComponent.sizeVariant" "small"))
    (doto field 
      (.putClientProperty "JComponent.sizeVariant" "small"))
    (doto panel
      (.add label)
      (.add field "pushx, growx, wrap"))))

(defn add-text-area [panel name value]
  (let [label (JLabel. name)
	area (JTextArea.)]
    (doto area
      (.setLineWrap true)
      (.setText value))
    (doto panel
      (.add label)
      (.add area "push, grow"))))

(defn create-log-tab [log]
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
	height (+ 200 (* (count semantics) (.. (JTextField.) (getPreferredSize) (getHeight))))]
    (doto panel
      (.setPreferredSize (Dimension. width height))
      (add-field "Name" name)
      (add-field "Location" location)
      (add-field "Depth Start" depth-start)
      (add-field "Depth End" depth-end)
      (add-field "Company" company)
      (add-field "Well" well)
      (add-field "Field" field)
      (add-field "Province/State" province-state)
      (add-field "County" county)
      (add-field "Country" country)
      (add-field "Well ID" well-id))))

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
  (let [frame (JFrame. "Inspector")
	panel (JPanel. (MigLayout. "ins 0"))
	content-panel (JPanel. (MigLayout. "ins 0"))
	tab-bar (init-tab-bar log-action format-action parameter-action)]
    (.. frame (getRootPane) (putClientProperty "Window.style" "small"))
    (doto panel 
      (.add tab-bar "wrap")
      (.add content-panel "push, grow"))
    (doto frame
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