(ns editor.controller
  (:require lasso
	    [editor.chart.controller :as chart-controller]
	    [editor.slider.controller :as slider-controller]
	    [editor.table.controller :as table-controller])
  (:use editor.model editor.view util global gutil curves)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane)
	   (org.jfree.data Range)
	   (org.jfree.chart ChartMouseListener)
	   (gui CustomChartPanel)))

(defn not-dragging-anything [editor]
  (dosync 
   (let [charts (:charts @editor)]
     (not-any? #(:dragged-entity (deref %)) charts))))

(defn init-frame [lasfile curves]
  (let [name (apply str (map #(str " | " (get-in (deref %) [:descriptor :mnemonic])) curves))
	frame (new JFrame (str (:name @lasfile) " " name))]
    frame))

(defn save [editor]
  (dosync 
   (doseq [chart (:charts @editor)]
     (let [dirty-curve (:dirty-curve @chart)
	   curve (:curve @chart)]
       (ref-set curve dirty-curve)))))

(defn init-merge-button [editor]
  (create-merge-button (fn [e] nil)))

(defn init-save-button [editor]
  (create-save-button (fn [e] (save editor))))

(defn init-tool-panel [slider table saveb mergeb]
  (create-panel 
   [(:widget @slider) "pushy, growy"]
   [(:pane @table) "pushy, growy, spanx 2, wrap"]
   [saveb "cell 1 1"]
   [mergeb "cell 2 1"]))

(defn init-main-panel [charts tool-panel width height]
  (let [panel (create-panelS
	       {:width width, :height height}
	       [tool-panel "pushy, growy"])]
    (swing 
     (doseq [chart charts]
       (.add panel (:chart-panel @chart) "pushx, pushy, growx, growy")))
    panel))

(defn open-curve-editor [lasfile curves]   
  (let [frame (init-frame lasfile curves)
	[index dirty-curves] (lasso/adjust-curves (map (comp lasso/deref-curve deref) curves))
	scale-notches 10
	editor (ref {})
	charts (for [[c d] (tuplize curves dirty-curves)]
		    (chart-controller/init-chart editor c d scale-notches))
	depth-data (:data index)
	slider-notches 200
	width (* 600 (count curves))
	height 700
	slider (slider-controller/init-slider slider-notches)
	table (table-controller/init-table index dirty-curves)
	editor-props (struct-map Editor
		       :frame frame
		       :lasfile lasfile
		       :index index 
		       :slider slider
		       :table table
		       :charts charts
		       :width width
		       :height height)
	saveb (init-save-button editor)
	mergeb (init-merge-button editor)
	tool-panel (init-tool-panel slider table saveb mergeb)
	main-panel (init-main-panel charts tool-panel width height)]
    (dosync 
     (ref-set editor editor-props))
    (swing
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    editor))