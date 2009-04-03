(ns editor.controller
  (:require lasso)
  (:use editor.model editor.view util global gutil curves storage)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane)
	   (org.jfree.data Range)
	   (org.jfree.chart ChartMouseListener)
	   (gui CustomChartPanel)))

(defn not-dragging-anything [frame]
  (let [charts (vals (lookup [frame :charts]))]
    (not-any? #(not (nil? (:dragged-entity %))) charts)))

(defn sync-chart-with-table [frame curve-id row]
  (dosync 
   (let [table (lookup-in [frame :widgets] :table)
	 chart (lookup-in [frame :charts] curve-id)
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

(defn sync-table-with-chart [frame curve-id index]
  (dosync 
   (let [table (lookup-in [frame :widgets] :table)
	 chart (lookup-in [frame :charts] curve-id)
	 {:keys [chart-panel table-column]} chart]
     (swing
      (let [model (.getModel table)
	    item (get-item chart-panel index)
	    row (index-to-row index table)
	    new-value (.getY item)]
	(.setValueAt model new-value row table-column)
	(.repaint table))))))

(defn sync-curve-with-chart [frame curve-id index]
  (swing-sync
   (let [chart-panel (lookup-in [frame :charts] curve-id :chart-panel)
	 item (get-item chart-panel index)
	 new-value (.getY item)]
     (update-dirty-curve frame curve-id index new-value))))

(defn sync-curve-with-table [frame curve row]
  (swing-sync  
   (let [table (lookup-in [frame :widgets] :table)
	 table-column (lookup-in [frame :charts] curve :table-column)
	 model (.getModel table)
	 new-value (.getValueAt model row table-column)
	 index (row-to-index row table)]
     (update-dirty-curve frame curve index 
			 (if (string? new-value)
			   (Double/valueOf new-value)
			   (double new-value))))))

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

(defn init-table-model-listener [frame curve-id]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (when (not-dragging-anything frame)
		    (sync-chart-with-table frame curve-id (.getFirstRow e))
		    (sync-curve-with-table frame curve-id (.getFirstRow e))))))

(defn init-slider-listener [frame]
  (proxy [ChangeListener] []
    (stateChanged 
     [event]
     (dosync 
      (let [{:keys [table depth-slider]} (lookup [frame :widgets])
	    {:keys [min-depth max-depth xaxes slider-notches] :as data} (lookup [frame :data])
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

(defn change-dragged-plot [frame curve-id chart-event]
  (swing
   (revise-in [frame :charts] [curve-id :dragged-entity] (.getEntity chart-event))))

(defn drag-plot [frame curve-id chart-event]
  (dosync 
   (let [{:keys [chart-panel dragged-entity]} (lookup-in [frame :charts] curve-id)
	 table (lookup-in [frame :widgets] :table)]	    
     (when dragged-entity
       (swing
	(let [mouse-event (.getTrigger chart-event)
	      series (retrieve-series chart-panel)
	      index (.getItem dragged-entity)
	      new-value (java-2D-to-value chart-panel (.getX mouse-event))]
	  (when (not (or (.isNaN new-value) (.isInfinite new-value)))
	    (.updateByIndex series index new-value)
	    (sync-table-with-chart frame curve-id index)
	    (sync-curve-with-chart frame curve-id index)
	    (.repaint chart-panel))))))))

(defn init-chart-mouse-listener [frame curve-id]
  (proxy [ChartMouseListener] []
    (chartMouseClicked [e] (change-dragged-plot frame curve-id e))
    (chartMouseMoved [e] (drag-plot frame curve-id e))))

(defn init-frame [lasfile curves]
  (let [name (apply str (map #(str " | " (get-in % [:descriptor :mnemonic])) curves))
	frame (new JFrame (str (:name lasfile) " " name))]
    frame))

(defn reset-xaxes [frame]
  (let [data (lookup [frame :data])
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

(defn save [frame]
  (dosync 
   (let [{:keys [index-id lasfile-id]} (lookup [frame :data])]
     (doseq [[curve-id chart] (lookup [frame :charts])]
       (let [dirty-curve (:dirty-curve chart)]
	 (revise curve-id 
		 (assoc dirty-curve
		   :index index-id)))))))

(defn init-merge-button [frame]
  (button "merge" (fn [e] nil)))

(defn init-save-button [frame]
  (button "save" (fn [e] (save frame))))

(defn get-charts [curve-ids dirty-curves]
  (apply merge 
	 (for [i (range 0 (count curve-ids))]
	   (let [curve-id (nth curve-ids i)
		 dirty-curve (nth dirty-curves i)
		 chart-panel (init-chart-panel dirty-curve)
		 tcolumn (inc i)]
	     {curve-id
	      (struct-map Chart
		:dirty-curve dirty-curve
		:chart-panel chart-panel
		:table-column tcolumn)}))))

(defn open-curve-editor [lasfile-id curve-ids]   
  (let [lasfile (lookup lasfile-id)
	curves (doall (map lookup curve-ids))
	frame (init-frame lasfile curves)
	[aggregate-index dirty-curves] (lasso/adjust-curves curves)
	index-id (store aggregate-index)
	curve-charts (get-charts curve-ids dirty-curves)
	plots (map #(.. (:chart-panel %)  (getChart) (getPlot)) (vals curve-charts))
	xaxes (map #(.getDomainAxis %) plots)
	depth-data (:data aggregate-index)
	data (struct-map FrameData
	       :lasfile-id lasfile-id
	       :index-id index-id
	       :min-depth (reduce min depth-data)
	       :max-depth (reduce max depth-data)
	       :slider-notches 200
	       :scale-notches 10
	       :xaxes xaxes
	       :width (* 600 (count curves))
	       :height 700)
	depth-slider (create-depth-slider (:slider-notches data))
	table (create-table aggregate-index dirty-curves)
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
     (store [frame :data] data)
     (store [frame :charts] curve-charts)
     (store [frame :widgets] widgets))
    (reset-xaxes frame)

    (.addChangeListener depth-slider (init-slider-listener frame))
    (doseq [[curve chart] curve-charts]
      (let [chart-panel (:chart-panel chart)]
	(.addChartMouseListener chart-panel (init-chart-mouse-listener frame curve))
	(.addTableModelListener (.getModel table) (init-table-model-listener frame curve))
	(swing (.add main-panel chart-panel "pushx, pushy, growx, growy"))))
    (table-show-percentage table 1)

    (swing
      (doto frame
	(.add main-panel)
	(.pack)
	(.setVisible true)))
    frame))
