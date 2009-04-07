(ns inspector.view
  (:use global gutil)
  (:import (javax.swing JFrame JPanel JLabel JTextField) 
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn add-field [panel ltext value method]
  (let [label (JLabel. ltext)
	field (JTextField. (str value))]
    (on-action field (method field))
    (doto panel
      (.add label)
      (.add field "pushx, growx, wrap"))))

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
	panel (JPanel. (MigLayout.))]
    (doto panel
      (.add (create-app-tab) "pushx, pushy, growx, growy"))
    (doto frame
      (.add panel)
      (.setResizable false)
      (.setSize (Dimension. width height))
      (.setVisible true))))
