(ns editor.controller
  (:load "/lasso")
  (:use editor.model editor.view util global gutil curves)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane)
	   (org.jfree.data Range)
	   (gui CustomChartPanel)))

(defstruct EditorState 
  :chart-states ; [ChartState]
  :data ; EditorData
  :widgets ; EditorWidgets
  )

(defstruct EditorWidgets 
  :table
  :chart-panels
  :depth-slider
  :main-panel)

(defstruct EditorData
  :lasfile
  :curves
  :index
  :min-depth
  :max-depth
  :scale-notches
  :slider-notches
  :xaxes)

(defstruct ChartState
  :curve
  :chart-panel
  :table-column
  :dragged-entity)

(def editor-states (agent {})) ;; frame -> editor-state

(defn retrieve-series [chart-panel]
  (first (.. chart-panel (getChart) (getPlot) (getDataset) (getSeries))))

(defn not-dragging-anything [frame]
  (let [chart-states (get-in @editor-states [frame :chart-states])]
    (not-any? #(not (nil? (:dragged-entity %))) chart-states)))

(defn sync-chart-with-table [frame chart-state row]
  (let [{:keys [chart-panel table-column]} chart-state
	table (get-in @editor-states [frame :widgets :table])
	model (.getModel table)
	series (retrieve-series chart-panel)
	index (row-to-index row table)
	value (.getValueAt model row table-column)]
    (swing 
     (.updateByIndex series index
		     (if (string? value)
		       (Double/valueOf value)
		       (double value)))
     (.repaint chart-panel))))

(defn sync-table-with-chart [frame chart-state index]
  (let [{:keys [chart-panel table-column]} chart-state
	table (get-in @editor-states [frame :widgets :table])
	model (.getModel table)
	chart (.getChart chart-panel)
	series (first (.. chart (getPlot) (getDataset) (getSeries)))
	row (index-to-row index table)
	item (.getDataItem series index)
	new-value (.getY item)]
    (swing 
     (.setValueAt model new-value row table-column)
     (.repaint table))))

(defn table-show-cell [table row col]
  (let [rect (.getCellRect table row col true)]
    (.scrollRectToVisible table rect)))

(defn table-show-percentage [table n]
  (guard (not (or (> n 1) (< n 0)))
	 (str "invalid n must be from 0.0 to 1.0: " n))
  (let [rows (dec (.getRowCount table))
	row (* n rows)]
    (table-show-cell table row 0)))

(defn init-table-model-listener [frame chart-state]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (when (not-dragging-anything frame)
		    (sync-chart-with-table frame chart-state (.getFirstRow e))))))

(defn init-slider-listener [editor-data editor-widgets]
  (let [{:keys [table depth-slider]} editor-widgets
	{:keys [min-depth max-depth xaxes slider-notches]} editor-data]
    (proxy [ChangeListener] []
      (stateChanged [event]
		    (let [scale (get-scale editor-data)
			  value (.getValue depth-slider)
			  scaled (scale-value editor-data value)
			  lower (+ scaled min-depth)
			  upper (+ lower scale)]
		      (doseq [xaxis xaxes]
			(.setRange xaxis
				   (with-limit (- max-depth scale) lower)
				   (with-limit max-depth upper)))
		      (table-show-percentage table (- 1 (/ value slider-notches))))))))

;(defn change-dragged-plot [frame old-chart-state chart-event]
;  (let [new-chart-state (assoc old-chart-state 
;			  :dragged-entity (.getEntity chart-event))]
;    (send entity-states
;	  (fn [es]
;	    (let [fs (get es frame)]
;	      (assoc es frame
;		     (assoc fs :chart-states (replace {old-chart-state new-chart-state} (:chart-states es)))))))))
;
;;(defn drag-plot [frame old-chart-state table chart-event])
;
;(defn init-chart-mouse-listener [chart-state table]
;  (proxy [ChartMouseListener] []
;    (chartMouseClicked [e] (change-dragged-plot chart-state e))
;    (chartMouseMoved [e] (drag-plot chart-state table e))))

(defn init-frame []
  (let [frame (new JFrame (str "Editor"))]
    (send editor-states assoc frame (struct EditorState))
    frame))

(defn configure-xaxes [editor-data]
  (let [{:keys [xaxes max-depth min-depth]} editor-data]
    (doseq [xaxis xaxes]
      (let [scale (get-scale editor-data)]
	(doto xaxis
	  (.setAutoRange false)
	  (.setRange (new Range min-depth (+ min-depth scale))))))))

(defn get-chart-states [curves]
  (for [i (range 0 (count curves))]
    (let [curve (nth curves i)
	  chart (create-chart curve)]
      (struct-map ChartState
	:curve curve
	:chart-panel (new CustomChartPanel chart)
	:table-column (inc i)))))

(defn open-curve-editor [lasfile curves]   
  (let [frame (init-frame)
	index (largest-index curves)
	padded-curves (map #(lasso/pad-curve index %) curves)
	chart-states (get-chart-states padded-curves)
	plots (map #(.getPlot (.getChart (:chart-panel %))) chart-states)
	xaxes (map #(.getDomainAxis %) plots)
	depth-data (:data index)
	editor-data (struct-map EditorData
		      :lasfile lasfile
		      :curves padded-curves
		      :index index
		      :min-depth (reduce min depth-data)
		      :max-depth (reduce max depth-data)
		      :slider-notches 200
		      :scale-notches 10
		      :xaxes xaxes)
	depth-slider (create-depth-slider editor-data)
	table (create-table editor-data)
	table-pane (new JScrollPane table)
	mergeb (create-merge-button editor-data)
	saveb (create-save-button editor-data)
	tool-panel (create-panel 
		    [depth-slider "pushy, growy"]
		    [table-pane "pushy, growy, wrap"]
		    [saveb ""]
		    [mergeb ""])
	main-panel (create-panelS
		    {:width (* 600 (count curves))
		     :height 700}
		    [tool-panel "pushy, growy"])
	editor-widgets (struct-map EditorWidgets
			 :table table
			 :chart-panels (map :chart-panel chart-states)
			 :depth-slider depth-slider
			 :main-panel main-panel)]
    (configure-xaxes editor-data)
    (doseq [state chart-states]
      (.add main-panel (:chart-panel state) "pushx, pushy, growx, growy"))
    (.addChangeListener depth-slider (init-slider-listener editor-data editor-widgets))

    (table-show-percentage table 1)
    (doto frame
      (.add main-panel)
      (.pack)
      (.setVisible true))
    ))
