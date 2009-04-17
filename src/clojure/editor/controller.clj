(ns editor.controller
  (:require lasso
	    [editor.slider.controller :as slider-controller]
	    [editor.table.controller :as table-controller]
	    [chart.controller :as chart-controller]
	    chart.panel)
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

(defn init-save-button [editor]
  (create-save-button (fn [e] (save editor))))

(defn init-tool-panel [slider table saveb]
  (create-panel 
   [(:widget @slider) "pushy, growy"]
   [(:pane @table) "pushy, growy, spanx 2, wrap"]
   [saveb "cell 1 1"]))

(defn init-main-panel [chart table-panel toolbar]
  (let [cpanel (JPanel. (MigLayout.))
	panel (create-panel
	       [table-panel "push, grow"])]
    (swing 
     (doto cpanel
       (.setBackground Color/white))
     (let [chart-panel (:chart-panel @chart)]
       (.add cpanel chart-panel "push, grow"))
     (doto panel
       (.setPreferredSize (Dimension. 700 900))
       (.add cpanel "push, grow")
       (.add toolbar "pushy, growy")))
    panel))

(def slider-watcher (agent nil))
(def chart-watcher (agent []))
(def table-watcher (agent []))

(defn scroll-table-and-chart [editor old-value slider]
  (dosync 
   (let [{:keys [table chart]} @editor
	 table-widget (:widget @table)
	 new-value (:value @slider)]
     (when (not= new-value old-value)
       (swing 
	(table-controller/show-percentage table-widget new-value)
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
	    (.setValueAt model value row 1)))))
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
	    (chart.panel/set-chart-value chart 0 index new-val))))
       [new-row old-val]))))

(defn init-zoom-button [editor]
  (let [button (JToggleButton. (ImageIcon. "resources/zoom.png"))]
    (on-action button
      (let [chart (:chart @editor)]
	(enable-zooming chart)))
    button))

(defn init-edit-button [editor]
  (let [button (JToggleButton. (ImageIcon. "resources/edit.png"))]
    (on-action button
      (let [chart (:chart @editor)]
	(enable-dragging chart)))
    button))

(defn init-toolbar [editor]
  (let [toolbar (JToolBar. JToolBar/VERTICAL)
	zoom-button (init-zoom-button editor)
	edit-button (init-edit-button editor)
	button-group (ButtonGroup.)]
    (.setSelected edit-button true)
    (doto button-group 
      (.add zoom-button)
      (.add edit-button))
    (doto toolbar
      (.setFloatable false)
      (.add zoom-button)
      (.add edit-button))))

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
	saveb (init-save-button editor)
	tool-bar (init-toolbar editor)
	tool-panel (init-tool-panel slider table saveb)
	main-panel (init-main-panel chart tool-panel tool-bar)]

    (dosync (ref-set editor editor-props))
    (chart-controller/enable-dragging chart)
    (add-watcher slider :send slider-watcher (partial scroll-table-and-chart editor))
    (add-watcher table :send table-watcher (partial sync-chart-with-table editor))
    (add-watcher chart :send chart-watcher (partial sync-table-with-chart editor))

    (swing
     (table-controller/show-percentage (:widget @table) 0)
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    editor))