(ns inspector.view
  (:use storage gutil)
  (:import (javax.swing JFrame JPanel JLabel JTextField) 
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn add-field [panel ltext value method]
  (let [label (new JLabel ltext)
	field (new JTextField (str value))]
    (on-action field (method field))
    (doto panel
      (.add label)
      (.add field "pushx, growx, wrap"))))

(defn create-app-tab [properties]
  (let [panel (JPanel. (MigLayout.))]
    (doto panel
      (add-field "Width" (lookup-in :app :width)
		 (fn [field]
		   (swing 
		    (let [value (Integer/valueOf (.getText field))]
		      (change-in :app [:width] value)))))
      (add-field "Height" (lookup-in :app :height)
		 (fn [field] 
		   (swing 
		    (let [value (Integer/valueOf (.getText field))]
		      (change-in :app [:height] value))))))))

(defn create-inspector-window [properties]
  (let [frame (new JFrame "Inspector")
	panel (JPanel. (MigLayout.))]
    (doto panel
      (.add (create-app-tab (:app-tab properties)) "pushx, pushy, growx, growy"))
    (doto frame
      (.add panel)
      (.setResizable false)
      (.setSize (Dimension. (:width properties) (:height properties)))
      (.setVisible true))))
