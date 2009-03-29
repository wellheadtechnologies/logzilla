(ns editor.controller
  (:load "/lasso")
  (:use editor.model util))

(defstruct EditorState 
  :chart-states ; [ChartState]
  :data ; EditorData
  :widgets ; EditorWidgets
  )

(defstruct EditorWidgets 
  :table
  :chart-panels
  :depth-slider)

(defstruct EditorData
  :lasfile
  :curves
  :index
  :min-depth
  :max-depth)

(defstruct ChartState
  :curve
  :chart-panel
  :table-column
  :dragged-entity)

(def editor-states (agent {})) ;; frame -> editor-state

(defn retrieve-series [chart-panel]
  (first (.. chart-panel (getChart) (getPlot) (getDataset) (getSeries))))

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
     (.setValueAt model new-value row column)
     (.repaint table))))

(defn table-show-cell [table row col]
  (let [rect (.getCellRect table row col true)]
    (.scrollRectToVisible table rect)))

(defn table-show-percentage [table n]
  (guard (or (> n 1) (< n 0))
	 (str "invalid n must be from 0.0 to 1.0: " n))
  (let [rows (dec (.getRowCount table))
	row (* n rows)]
    (table-show-cell table row 0)))

(defn table-model-listener [frame chart-state]
  (proxy [TableModelListener] []
    (tableChanged [e]
		  (guard (= (.getFirstRow e) (.getLastRow e))
			 "first row must equal last row")
		  (when (not-dragging-anything)
		    (sync-chart-with-table frame chart-state (.getFirstRow e))))))

(defn slider-listener [frame]
  (let [scale (get-scale max-depth min-depth)]
    (proxy [ChangeListener] []
      (stateChange [event]
		   (let [value (.getValue depth-slider)
			 scaled (scale-value scale value)
			 lower (+ scaled min-depth)
			 upper (+ lower scale)]
		     (doseq [xaxis xaxes]
		       (.setRange xaxis
				  (with-limit (- max-depth scale) lower)
				  (with-limit max-depth upper))))
		   (table-show-percentage table (- 1 (/ value slider-n)))))))

(defn init-frame []
  (let [frame (new JFrame (str "Editor"))]
    (send editor-states assoc frame (struct EditorState))
    frame))

(defn open-curve-editor [lasfile curves]   
  (let [frame (init-frame)
	index (largest-index curves)
	padded-curves (map #(lasso/pad-curve index %) curves)
	depth-data (:data index)
	editor-data (struct-map EditorData
		      :lasfile lasfile
		      :curves curves
		      :index 
		      :min-depth (reduce min depth-data)
		      :max-depth (reduce max depth-data)
		      :slider-notches 200)
	depth-slider (create-depth-slider min-depth max-depth)
	charts (map curve-to-chart padded-curves)
	table (create-table index padded-curves)
	chart-panels (map (fn [[curve chart]]
			    (new CustomChartPanel curve chart))
			  (tuplize padded-curves charts))
	plots (map #(.getPlot %) chart)
	x-axes (map #(.getDomainAxis %) plots)
	mergeb (create-merge-button lasfile index padded-curves)
	saveb (create-save-button lasfile padded-curves )
	tool-panel (create-panel 
		    [depth-slider "pushy, growy"]
		    [table-pane "pushy, growy, wrap"]
		    [saveb ""]
		    [mergeb ""])
	main-panel (create-panelS
		    {:width (* 600 (count chart-panels))
		     :height 700}
		    [tool-panel "pushy, growy"])]

    (configure-xaxes x-axes max-depth min-depth)
    (add-chart-panels main-panel table chart-panels)
    (.addChangeListener depth-slider 
			(create-slider-listener depth-slider
						x-axes max-depth
						min-depth table))

    (doto table
      (.showAtPercentage 1))
    (doto frame
      (.add main-panel)
      (.pack)
      (.setVisible true))
    ))
