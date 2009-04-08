(ns inspector.view
  (:use global gutil)
  (:import (javax.swing JFrame JPanel JLabel JTextField JTabbedPane) 
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn create-app-tab []
  (let [panel (JPanel. (MigLayout.))]
    (doto panel
      (add-field "Width" (:width @app)
		 (fn [field]
		   (swing 
		    (let [value (Integer/valueOf (.getText field))]
		      (dosync (alter app assoc :width value))))))
      (add-field "Height" (:height @app)
		 (fn [field] 
		   (swing 
		    (let [value (Integer/valueOf (.getText field))]
		      (dosync (alter app assoc :height value)))))))))

(defn create-curves-tab [curves]
  (let [panel (JPanel. (MigLayout.))
	curve @(first curves)
	icon (:icon curve)
	from-descriptor #(get-in curve [:descriptor %])]
    (doto panel
      (.add icon "spanx 2, wrap")
      (add-field "Mnemonic" (from-descriptor :mnemonic) (fn [e] nil))
      (add-field "Unit" (from-descriptor :unit) (fn [e] nil))
      (add-text-area "Description" (from-descriptor :description) (fn [e] nil)))))

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
