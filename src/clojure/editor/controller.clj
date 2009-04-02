(ns editor.controller
  (:require lasso)
  (:use editor.model editor.view util global gutil curves)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane)
	   (org.jfree.data Range)
	   (org.jfree.chart ChartMouseListener)
	   (gui CustomChartPanel)))

(defn not-dragging-anything [frame]
  (let [charts (vals (get @frame-charts frame))]
    (not-any? #(not (nil? (:dragged-entity %))) charts)))

(defn sync-chart-with-table [frame curve row]
  (dosync 
   (let [table (get-in @frame-widgets [frame :table])
	 chart (get-in @frame-charts [frame curve])
	 {:keys [chart-panel table-column]} chart]
     (swing 
      (let [model (.getModel table)
	    value (.getValueAt model row table-column)
	    series (retrieve-series chart-panel)
	    index (row-to-index row table)]
	(.updateByIndex series index
			(if (string? value)
			  (Double/valueOf value)
			  (double value)))
	(.repaint chart-panel))))))

(defn sync-table-with-chart [frame curve index]
  (dosync 
   (let [chart (get-in @frame-charts [frame curve])
	 table (get-in @frame-widgets [frame :table])
	 {:keys [chart-panel table-column]} chart]
     (swing 
      (let [model (.getModel table)
	    chart (.getChart chart-panel)
	    series (first (.. chart (getPlot) (getDataset) (getSeries)))
	    row (index-to-row index table)
	    item (.getDataItem series index)
	    new-value (.getY item)]
	(.setValueAt model new-value row table-column)
	(.repaint table))))))

(defn table-show-cell [table row col]
  (swing 
   (let [rect (.getCellRect table row col true)]
     (.scrollRectToVisible table rect))))

(defn table-show-percentage [table n]
  (swing   
   (guard (not (or (> n 1) (< n 0)))
	  (str "invalid n must be from 0.0 to 1.0: " n))
   (let [rows (dec (.getRowCount table))
	 row (* n rows)]
     (table-show-cell table row 0))))

(defn init-table-model-listener [frame curve]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (when (not-dragging-anything frame)
		    (sync-chart-with-table frame curve (.getFirstRow e))))))

(defn init-slider-listener [frame]
  (proxy [ChangeListener] []
    (stateChanged 
     [event]
     (dosync 
      (let [{:keys [table depth-slider]} (get @frame-widgets frame)
	    {:keys [min-depth max-depth xaxes slider-notches] :as data} (get @frame-data frame)
	    scale (get-scale data)]
	(swing 
	 (let [value (.getValue depth-slider)
	       scaled (scale-value data value)
	       lower (+ scaled min-depth)
	       upper (+ lower scale)]
	   (doseq [xaxis xaxes]
	     (.setRange xaxis
			(with-limit (- max-depth scale) lower)
			(with-limit max-depth upper)))
	   (table-show-percentage table (- 1 (/ value slider-notches))))))))))

(defn change-dragged-plot [frame curve chart-event]
  (swing
   (dosync 
    (let [old-chart (get-in @frame-charts [frame curve])
	  new-chart (assoc old-chart :dragged-entity (.getEntity chart-event))]
      (alter frame-charts assoc-in [frame curve] new-chart)))))

(defn drag-plot [frame curve chart-event]
  (dosync 
   (let [chart (get-in @frame-charts [frame curve])
	 table (get-in @frame-widgets [frame :table])
	 dragged-entity (:dragged-entity chart)
	 chart-panel (:chart-panel chart)]	    
     (when dragged-entity
       (swing
	(let [mouse-event (.getTrigger chart-event)
	      series (retrieve-series chart-panel)
	      index (.getItem dragged-entity)
	      new-value (java-2D-to-value chart-panel (.getX mouse-event))]
	  (when (not (or (.isNaN new-value) (.isInfinite new-value)))
	    (.updateByIndex series index new-value)
	    (sync-table-with-chart frame curve index)
	    (.repaint chart-panel))))))))

(defn init-chart-mouse-listener [frame curve]
  (proxy [ChartMouseListener] []
    (chartMouseClicked [e] (change-dragged-plot frame curve e))
    (chartMouseMoved [e] (drag-plot frame curve e))))

(defn init-frame [lasfile curves]
  (let [name (apply str (map #(str " | " (get-in % [:descriptor :mnemonic])) curves))
	frame (new JFrame (str (:name lasfile) " " name))]
    frame))

(defn reset-xaxes [frame]
  (let [data (get @frame-data frame)
	{:keys [xaxes max-depth min-depth]} data
	scale (get-scale data)]
    (swing 
     (doseq [xaxis xaxes]
       (doto xaxis
	 (.setAutoRange false)
	 (.setRange (new Range min-depth (+ min-depth scale))))))))

(defn init-chart-panel [curve]
  (let [chart (create-chart curve)
	chart-panel (new CustomChartPanel chart)]
    (doto chart-panel
      (.setDomainZoomable false)
      (.setMouseZoomable false))))

(defn init-merge-button [frame]
  (button "merge" (fn [e] nil)))

(defn init-save-button [frame]
  (button "save" (fn [e] nil)))

(defn get-charts [curves]
  (apply merge 
   (for [i (range 0 (count curves))]
     (let [curve (nth curves i)
	   chart-panel (init-chart-panel curve)
	   tcolumn (inc i)]
       {curve
	(struct-map Chart
	  :chart-panel chart-panel
	  :table-column tcolumn)}))))

(defn open-curve-editor [lasfile curves]   
  (let [frame (init-frame lasfile curves)
	[aggregate-index adjusted-curves] (lasso/adjust-curves curves)
	curve-charts (get-charts adjusted-curves)
	plots (map #(.. (:chart-panel %)  (getChart) (getPlot)) (vals curve-charts))
	xaxes (map #(.getDomainAxis %) plots)
	depth-data (:data aggregate-index)
	data (struct-map FrameData
		      :lasfile lasfile
		      :index aggregate-index
		      :min-depth (reduce min depth-data)
		      :max-depth (reduce max depth-data)
		      :slider-notches 200
		      :scale-notches 10
		      :xaxes xaxes
		      :width (* 600 (count curves))
		      :height 700)
	depth-slider (create-depth-slider (:slider-notches data))
	table (create-table aggregate-index adjusted-curves)
	table-pane (new JScrollPane table)
	saveb (init-save-button frame)
	mergeb (init-merge-button frame)
	tool-panel (create-panel 
		    [depth-slider "pushy, growy"]
		    [table-pane "pushy, growy, spanx 2, wrap"]
		    [saveb "cell 1 1"]
		    [mergeb "cell 2 1"])
	main-panel (create-panelS
		    {:width (:width data)
		     :height (:height data)}
		    [tool-panel "pushy, growy"])
	widgets (struct-map FrameWidgets
		  :table table
		  :depth-slider depth-slider
		  :main-panel main-panel)]
    (dosync 
     (alter frame-charts assoc frame curve-charts)
     (alter frame-data assoc frame data)
     (alter frame-widgets assoc frame widgets))
    (reset-xaxes frame)
    (swing 
     (.addChangeListener depth-slider (init-slider-listener frame))
     (doseq [[curve chart] curve-charts]
       (let [chart-panel (:chart-panel chart)]
	 (.addChartMouseListener chart-panel (init-chart-mouse-listener frame curve))
	 (.addTableModelListener (.getModel table) (init-table-model-listener frame curve))
	 (.add main-panel chart-panel "pushx, pushy, growx, growy"))))

    (table-show-percentage table 1)
    (swing
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    ))
