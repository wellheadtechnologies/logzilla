(ns editor.controller
  (:require lasso
	    [editor.chart.controller :as chart-controller]
	    [editor.slider.controller :as slider-controller]
	    [editor.table.controller :as table-controller])
  (:use editor.model editor.view util global gutil curves storage)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane)
	   (org.jfree.data Range)
	   (org.jfree.chart ChartMouseListener)
	   (gui CustomChartPanel)))

(defn not-dragging-anything [editor-id]
  (dosync 
   (let [chart-ids (lookup-in editor-id :chart-ids)
	 charts (map lookup chart-ids)]
     (not-any? #(:dragged-entity %) charts))))

(defn init-frame [lasfile curves]
  (let [name (apply str (map #(str " | " (get-in % [:descriptor :mnemonic])) curves))
	frame (new JFrame (str (:name lasfile) " " name))]
    frame))

(defn save [editor-id]
  (dosync 
   (doseq [chart-id (lookup-in editor-id :charts)
	   index-id (lookup-in editor-id :index-id)]
     (let [dirty-curve (lookup-in chart-id :dirty-curve)
	   curve-id (lookup-in chart-id :curve-id)]
       (change curve-id 
	       (assoc dirty-curve
		 :index index-id))))))

(defn init-merge-button [editor-id]
  (create-merge-button (fn [e] nil)))

(defn init-save-button [editor-id]
  (create-save-button (fn [e] (save editor-id))))

(defn init-tool-panel [dslider table saveb mergeb]
  (create-panel 
   [dslider "pushy, growy"]
   [(JScrollPane. table) "pushy, growy, spanx 2, wrap"]
   [saveb "cell 1 1"]
   [mergeb "cell 2 1"]))

(defn init-main-panel [charts tool-panel width height]
  (let [panel (create-panelS
	       {:width width, :height height}
	       [tool-panel "pushy, growy"])]
    (swing 
     (doseq [chart charts]
       (.add panel (:chart-panel chart) "pushx, pushy, growx, growy")))
    panel))

(defn get-instance-properties [editor-id lf-id index-id dslider table chart-ids width height]
  (instance-properties 
   [:lasfile-id lf-id]
   [:index-id index-id]
   [:depth-slider dslider]
   [:table table]
   [:chart-ids chart-ids]
   [:width width]
   [:height height]
   [:not-dragging-anything (partial not-dragging-anything editor-id)]))

(defn open-curve-editor [lasfile-id curve-ids]   
  (let [lasfile (lookup lasfile-id)
	curves (doall (map lookup curve-ids))
	frame (init-frame lasfile curves)
	[aggregate-index dirty-curves] (lasso/adjust-curves curves)
	index-id (store aggregate-index)
	scale-notches 10
	chart-ids (for [[c d] (tuplize curve-ids dirty-curves)]
		    (chart-controller/init-chart frame c d scale-notches))
	charts (doall (map lookup chart-ids))
	depth-data (:data aggregate-index)
	slider-notches 200
	width (* 600 (count curves))
	height 700
	depth-slider (slider-controller/init-slider frame slider-notches)
	table (table-controller/init-table aggregate-index dirty-curves)
	props (get-instance-properties frame lasfile-id index-id depth-slider table
				       chart-ids width height)
	saveb (init-save-button frame)
	mergeb (init-merge-button frame)
	tool-panel (init-tool-panel depth-slider table saveb mergeb)
	main-panel (init-main-panel charts tool-panel width height)]
    (dosync 
     (store-properties frame props)
     (on-change depth-slider [:value] #(swing (table-controller/show-percentage table %)))
     (on-change depth-slider [:value] #(swing (chart-controller/show-percentage chart-ids %))))
    (swing
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    frame))