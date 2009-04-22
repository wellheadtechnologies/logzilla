(ns editor.controller
  (:require lasso
	    [editor.slider.controller :as slider-controller]
	    [editor.table.controller :as table-controller]
	    [chart.controller :as chart-controller])
  (:use editor.model editor.view util global gutil curves chart.controller messages)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane JToolBar JButton JToggleButton 
			ButtonGroup ImageIcon JPanel
			ScrollPaneConstants)
	   (java.awt Dimension Color Toolkit Image Point)
	   (javax.imageio ImageIO)
	   (java.io File)
	   (org.jfree.data Range)
	   (org.jfree.chart ChartMouseListener)
	   (net.miginfocom.swing MigLayout)))

(defn not-dragging-anything [editor]
  (dosync
   (let [chart (:chart @editor)
	 dragged-entity (:dragged-entity @chart)]
     (nil? dragged-entity))))

(defn init-frame [lasfile curve]
  (let [name (get-in @curve [:descriptor :mnemonic])
	frame (new JFrame (str (:name @lasfile) " " name))]
    frame))

(defn save [editor]
  (dosync
   (let [chart (:chart @editor)]
     (chart-controller/save-chart chart))))

(defn init-table-panel [slider table]
  (let [panel (JPanel. (MigLayout. "ins 0"))]
    (doto panel
      (.add (:widget @slider) "pushy, growy")
      (.add (:pane @table) "push, grow"))))

(defn init-main-panel [chart table-panel left-toolbar right-toolbar]
  (let [left-panel (JPanel. (MigLayout. "ins 0"))
	right-panel (JPanel. (MigLayout. "ins 0"))
	main-panel (JPanel. (MigLayout. "ins 0"))]
    (doto left-panel
      (.add table-panel "push, grow"))
    
    (let [chart-panel (:chart-panel @chart)]
      (doto right-panel
	(.add right-toolbar "pushx, growx, wrap")
	(.add chart-panel "push, grow")))

    (doto main-panel
      (.setPreferredSize (Dimension. 700 900))
      (.add left-panel "width 35%, pushy, growy")
      (.add right-panel "width 65%, pushy, growy"))
    main-panel))

(defn update-canonical-percentage [editor percentage]
  (dosync
   (let [{:keys [slider table chart canonical-percentage]} @editor]
     (when (not= canonical-percentage percentage)
       (alter editor assoc :canonical-percentage percentage)
       (swing 
	(ignore :percentage-change slider (slider-controller/set-percentage slider percentage))
	(ignore :percentage-change table (table-controller/show-percentage table percentage))
	(ignore :percentage-change chart (chart-controller/show-percentage chart percentage)))))))

(defn update-table [table event]
  (dosync
   (let [{:keys [row value]} event
	 table-widget (:widget @table)]
     (swing 
      (let [model (.getModel table-widget)]
	(ignore :value-change table (.setValueAt model value row 1))
	(table-controller/show-cell table-widget row 1))))))

(defn update-chart [chart event]
  (dosync 
   (let [{:keys [index value]} event]
     (swing
       (let [value (convert-to-double value)]
	 (ignore :value-change chart (chart-controller/set-chart-value chart 0 index value)))))))

(defn init-save-button [editor]
  (create-save-button #(save editor)))

(defn init-zoom-button [editor]
  (create-zoom-button
   #(let [chart (:chart @editor)]
      (dosync
       (disable-panning chart)
       (disable-dragging chart)
       (enable-zooming chart)))))

(defn init-edit-button [editor]
  (create-edit-button
   #(let [chart (:chart @editor)]
      (dosync
       (disable-panning chart)
       (disable-zooming chart)
       (enable-dragging chart)))))

(defn init-reset-button [editor]
  (create-reset-button
   #(let [chart (:chart @editor)]
      (reset chart))))

(defn init-points-button [editor]
  (create-points-button
   #(let [chart (:chart @editor)]
      (toggle-points chart))))

(defn init-pan-button [editor]
  (create-pan-button
   #(let [chart (:chart @editor)]
      (dosync
       (disable-zooming chart)
       (disable-dragging chart)
       (toggle-panning chart)))))

(defn init-left-toolbar [editor]
  (let [toolbar (JToolBar. JToolBar/HORIZONTAL)]
    (doto toolbar
      (.setFloatable false))))

(defn init-right-toolbar [editor]
  (let [toolbar (JToolBar. JToolBar/HORIZONTAL)
	zoom-button (init-zoom-button editor)
	edit-button (init-edit-button editor)
	reset-button (init-reset-button editor)
	points-button (init-points-button editor)
	pan-button (init-pan-button editor)
	button-group (ButtonGroup.)]
    (.setSelected edit-button true)
    (doto button-group
      (.add zoom-button)
      (.add edit-button)
      (.add pan-button))
    (doto toolbar
      (.setFloatable false)
      (.add zoom-button)
      (.add edit-button)
      (.add pan-button)
      (.add reset-button)
      (.add points-button))))

(defn open-curve-editor [lasfile curve]   
  (let [frame (init-frame lasfile curve)
	dirty-curve (lasso/deref-curve @curve)
	index (:index dirty-curve)
	editor (ref {})
	chart (chart-controller/init-chart curve dirty-curve)
	depth-data (:data index)
	slider-notches 200
	slider (slider-controller/init-slider slider-notches)
	table (table-controller/init-table index dirty-curve)
	editor-props (struct-map Editor
		       :frame frame
		       :lasfile lasfile
		       :index index 
		       :slider slider
		       :table table
		       :chart chart)
	left-tool-bar (init-left-toolbar editor)
	right-tool-bar (init-right-toolbar editor)
	table-panel (init-table-panel slider table)
	main-panel (init-main-panel chart table-panel left-tool-bar right-tool-bar)]

    (dosync (ref-set editor editor-props))
    (chart-controller/enable-dragging chart)

    (add-listener :percentage-change slider 
		  (fn [event]
		    (update-canonical-percentage editor (:percentage event))))

    (add-listener :percentage-change chart 
		  (fn [event]
		    (update-canonical-percentage editor (:percentage event))))

    (add-listener :value-change chart 
		  (fn [event] 
		    (dosync
		     (let [{:keys [data-index value]} event
			   row (index-to-row (:widget @table) data-index)]
		       (update-table table {:row row :value value})))))
    (add-listener :value-change table 
		  (fn [event]
		    (dosync
		     (let [{:keys [row value]} event
			   index (row-to-index (:widget @table) row)]
		       (update-chart chart {:index index :value value})))))

    (swing
     (update-canonical-percentage editor 0)
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    editor))