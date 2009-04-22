(ns inspector.view
  (:use global gutil semantics inspector.model)
  (:import (javax.swing JFrame JPanel JLabel JTextArea
			JTextField JTabbedPane JButton
			JToolBar ButtonGroup JButton 
			JToggleButton Box) 
	   (java.awt Dimension Color)
	   (net.miginfocom.swing MigLayout)))

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