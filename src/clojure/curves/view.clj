(ns gui.curves
  (:use curves.controller curves.model))

(def open-curve-editor)

(defn- merge-button [lasfile index curves]
  (button "Merge" 
    (fn [e]
      (open-curve-editor lasfile [(merge-curves index curves)]))))

(defn- save-button [lasfile curves]
  (button "Save"
    (fn [e]
      (save-curves lasfile (dirty-curves-for curves)))))

(defn- create-depth-slider [min-depth max-depth]
  (let [slider (new JSlider 0 slider-n 0)]
    (.setOrientation slider JSlider/VERTICAL)
    slider))

(defn table-show-cell [table row col]
  (let [rect (.getCellRect table row coll true)]
    (.scrollRectToVisible table rect)))

(defn table-show-percentage [table n]
  (guard (or (> n 1) (< n 0))
	 "invalid n (must be from 0.0 to 1.0)")
  (let [rows (dec (.getRowCount table))
	row (* n rows)]
    (.showCell table row 0)))

(defn- create-table [index curves]
  (let [table (new JTable)
	model (new DefaultTableModel)
	index-data (large-to-small (.getLasData index))
	do-reverse (not= (first index-data) (first (.getLasData index)))
	curve-datas (if do-reverse
		      (map #(reverse (:data %)) curves)
		      (map :data curves))]
    
    (.addColumn model (:mnemonic index) (into-array Object index-data))

    (doseq [curve curves]
      (adjust-curve index curve))

    (doseq [[curve curve-data] (tuplize curves curve-datas)]
      (let [curve-index-data (large-to-small (:data (:index curve)))
	    srate (isample-rate index)
	    begin-padding (repeat (start-offset index-data curve-index-data srate) 0)
	    after-padding (repeat (end-offset index-data curve-index-data srate) 0)]
	(.addColumn model (.getMnemonic curve) 
		    (into-array Object (concat 
					begin-padding
					curve-data
					after-padding)))
	))
    (.setModel table model)
    table))

(defn open-curve-editor [lasfile curves]
  (guard (all-samef (map sample-rate curves))
	 "Sample rates must be the same")

  (let [index (largest-index curves)
	acurves (map #(adjust-curve index %) curves)
	depth-data (.getLasData index)
	min-depth (reduce min depth-data)
	max-depth (reduce max depth-data)
	depth-slider (create-depth-slider min-depth max-depth)
	charts (map curve-to-chart acurves)
	table (create-table index acurves)
	table-pane (new JScrollPane table)
	chart-panels (map (fn [[curve chart]]
			    (new CustomChartPanel curve chart))
			  (tuplize acurves charts))
	frame (new JFrame (str "Curves Editor"))
	plots (map #(.getPlot %) charts)
	x-axes (map #(.getDomainAxis %) plots)
	mergeb (merge-button lasfile index acurves)
	saveb (save-button lasfile acurves)

	tool-panel (panel 
		    [depth-slider "pushy, growy"]
		    [table-pane "pushy, growy, wrap"]
		    [saveb ""]
		    [mergeb ""])

	main-panel (panelS 
		    (* 600 (count chart-panels)) 700
		    [tool-panel "pushy, growy"])
	]

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
    
    frame))