(ns editor.controller
  (:require lasso
	    [editor.slider.controller :as slider-controller]
	    [editor.table.controller :as table-controller]
	    [chart.controller :as chart-controller])
  (:use editor.model editor.view util global gutil curves chart.controller)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane JToolBar JButton JToggleButton 
			ButtonGroup ImageIcon JPanel
			ScrollPaneConstants)
	   (java.awt Dimension Color)
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
    (swing 
     (doto left-panel
       (.add table-panel "push, grow"))

     (let [chart-panel (:chart-panel @chart)]
       (doto right-panel
	 (.add right-toolbar "pushx, growx, wrap")
	 (.add chart-panel "push, grow")))

     (doto main-panel
       (.setPreferredSize (Dimension. 700 900))
       (.add left-panel "width 35%, pushy, growy")
       (.add right-panel "width 65%, pushy, growy")))
    main-panel))

(def slider-watcher (agent nil))
(def chart-watcher (agent []))
(def table-watcher (agent []))
(def percentage-watcher (agent nil))

(defn scroll-table-and-chart [editor old-value slider]
  (dosync 
   (let [{:keys [table chart]} @editor
	 new-value (:value @slider)]
     (when (not= new-value old-value)
       (swing 
	(table-controller/show-percentage table new-value)
	(chart-controller/show-percentage chart new-value)))
     new-value)))

(defn sync-table-with-chart [editor [old-index old-value] chart]
  (dosync
   (let [table (:table @editor)
	 table-widget (:widget @table)
	 [curve-index data-index] (:changes @chart)
	 dirty-curve (only (:dirty-curves @chart))
	 value (get-in dirty-curve [:data data-index])]
     (when (and (not= nil data-index) 
		(not= nil value))
       (guard (= curve-index 0) 
	      "curve index must be zero, as the editor only edits one curve at a time")
       (swing 
	(let [model (.getModel table-widget)
	      row (index-to-row data-index table-widget)]
	  (when (or (not= old-index data-index)
		    (not= old-value value)) 
	    (.setValueAt model value row 1)
	    (table-controller/show-cell table-widget row 1)))))
     [data-index value])))

(defn sync-chart-with-table [editor [old-row old-val] table]
  (dosync 
   (when (not-dragging-anything editor)
     (let [new-row (:altered-row @table)
	   new-val (:altered-val @table)
	   chart (:chart @editor)]
       (when (and (not-any? nil? [new-row new-val])
		  (or (not= old-row new-row)
		      (not= old-val new-val)))
	 (swing
	  (let [index (row-to-index new-row (:widget @table))
		new-val (convert-to-double new-val)]
	    (chart-controller/set-chart-value chart 0 index new-val))))
       [new-row old-val]))))

(defn init-save-button [editor]
  (create-save-button #(save editor)))

(defn init-zoom-button [editor]
  (create-zoom-button
   #(let [chart (:chart @editor)]
      (enable-zooming chart))))

(defn init-edit-button [editor]
  (create-edit-button
   #(let [chart (:chart @editor)]
      (enable-dragging chart))))

(defn init-reset-button [editor]
  (create-reset-button
   #(let [chart (:chart @editor)]
      (reset chart))))

(defn init-left-toolbar [editor]
  (let [toolbar (JToolBar. JToolBar/HORIZONTAL)]
    (doto toolbar
      (.setFloatable false))))

(defn init-right-toolbar [editor]
  (let [toolbar (JToolBar. JToolBar/HORIZONTAL)
	zoom-button (init-zoom-button editor)
	edit-button (init-edit-button editor)
	reset-button (init-reset-button editor)
	button-group (ButtonGroup.)]
    (.setSelected edit-button true)
    (doto button-group
      (.add zoom-button)
      (.add edit-button))
    (doto toolbar
      (.setFloatable false)
      (.add zoom-button)
      (.add edit-button)
      (.add reset-button))))

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
    (add-watcher slider :send slider-watcher (partial scroll-table-and-chart editor))
    (add-watcher table :send table-watcher (partial sync-chart-with-table editor))
    (add-watcher chart :send chart-watcher (partial sync-table-with-chart editor))

    (swing
     (table-controller/show-percentage table 0)
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    editor))