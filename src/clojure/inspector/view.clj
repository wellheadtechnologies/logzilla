(ns inspector.view
  (:use global gutil)
  (:import (javax.swing JFrame JPanel JLabel JTextArea
			JTextField JTabbedPane JButton) 
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn create-app-tab []
  (let [panel (JPanel. (MigLayout.))
	width-field (JTextField. (:width @app))
	height-field (JTextField. (:height @app))]
    (doto panel
      (.add (JLabel. "Width"))
      (.add width-field "pushx, growx, wrap")

      (.add (JLabel. "Height"))
      (.add height-field "pushx, growx, wrap"))))

(defn create-curves-tab [curves]
  (let [panel (JPanel. (MigLayout.))
	curve-ref (first curves)
	curve @curve-ref
	icon (:icon curve)
	from-descriptor #(get-in curve [:descriptor %])
	mnemonic-field (JTextField. (from-descriptor :mnemonic))
	unit-field (JTextField. (from-descriptor :unit))
	description-area (JTextArea. (from-descriptor :description))
	change-button (JButton. "Change")]
    (doto description-area
      (.setLineWrap true))
    (doto change-button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action 
	  (let [m (.getText mnemonic-field)
		u (.getText unit-field)
		d (.getText description-area)]
	    (dosync 
	     (alter curve-ref assoc-in [:descriptor :mnemonic] m)
	     (alter curve-ref assoc-in [:descriptor :unit] u)
	     (alter curve-ref assoc-in [:descriptor :description] d)))))
    (doto panel
      (.add icon "spanx 2, wrap")

      (.add (JLabel. "Mnemonic"))
      (.add mnemonic-field "pushx, growx, wrap")
      
      (.add (JLabel. "Unit"))
      (.add unit-field "pushx, growx, wrap")
      
      (.add (JLabel. "Description") "spanx 2, wrap")
      (.add description-area "spanx 2, push, grow, wrap")
      (.add change-button "align 50%, spanx 2, wrap"))
    ))

(defn create-inspector-window [width height]
  (let [frame (JFrame. "Inspector")
	panel (JTabbedPane.)]
    (doto panel
      (.addTab "App" (create-app-tab)))
    (doto frame
      (.add panel)
      (.setSize (Dimension. width height)))
    {:frame frame
     :panel panel}))
