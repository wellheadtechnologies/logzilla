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

(defn create-inspector-window [width height]
  (let [frame (JFrame. "Inspector")
	panel (JTabbedPane.)]
    (doto panel
      (.addTab "App" (create-app-tab)))
    (doto frame
      (.add panel)
      (.setSize (Dimension. width height))
      (.setVisible true))))
