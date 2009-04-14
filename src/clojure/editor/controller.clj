(ns editor.controller
  (:require lasso
	    [chart.controller :as chart-controller]
	    [editor.slider.controller :as slider-controller]
	    [editor.table.controller :as table-controller]
	    chart.panel)
  (:use editor.model editor.view util global gutil curves)
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
       (alter curve assoc :data (:data dirty-curve))))))

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

(defn init-main-panel [charts tool-panel toolbar]
  (let [charts-panel (JPanel. (MigLayout.))
	chart-pane (JScrollPane. charts-panel)
	width (with-limit 1000 (* 400 (count charts)))
	height 700
	panel (create-panel
	       [tool-panel "width 30%, height 100%"])]
    (swing 
     (doto chart-pane
       (.setBackground Color/white))
     (doto charts-panel
       (.setBackground Color/white))
     (doseq [chart charts]
       (let [chart-panel (:chart-panel @chart)]
	 (.add charts-panel chart-panel (str "width 350, height " (- height 50)))))
     (doto panel
       (.add chart-pane (str "width " width ", height " height ", push, grow"))
       (.add toolbar "pushy, growy")))
    panel))

(def slider-watcher (agent nil))
(def chart-watcher (agent []))
(def table-watcher (agent []))

(defn scroll-table-and-chart [editor old-value slider]
  (dosync 
   (let [table (get @editor :table)
	 table-widget (get @table :widget)
	 charts (get @editor :charts)
	 new-value (:value @slider)]
     (when (not= new-value old-value)
       (swing 
	(table-controller/show-percentage table-widget new-value)
	(chart-controller/show-percentage charts new-value)))
     new-value)))

(defn sync-table-with-chart [editor column [old-index old-value] chart]
  (dosync
   (let [table (get @editor :table)
	 table-widget (get @table :widget)
	 index (get @chart :changed-index)
	 value (get-in @chart [:dirty-curve :data index])]
     (when (and (not= nil index)
		(not= nil value))
       (swing 
	(let [model (.getModel table-widget)
	      row (index-to-row index table-widget)]
	  (cond
	   (not= old-index index) (.setValueAt model value row column)
	   (not= old-value value) (.setValueAt model value row column)))))
     [index value])))

(defn sync-chart-with-table [editor [old-row old-col old-val] table]
  (dosync 
   (when (not-dragging-anything editor)
     (let [new-row (:altered-row @table)
	   new-col (:altered-col @table)
	   new-val (:altered-val @table)
	   chart (nth (:charts @editor) (dec new-col))]
       (when (and (not-any? nil? [new-row new-col new-val])
		  (or (not= old-col new-col)
		      (not= old-row new-row)
		      (not= old-val new-val)))
	 (swing
	  (let [index (row-to-index new-row (:widget @table))
		new-val (convert-to-double new-val)]
	    (chart.panel/set-chart-value chart index new-val))))
       [new-row new-col old-val]))))

(defn init-zoom-button [editor]
  (let [button (JToggleButton. (ImageIcon. "resources/zoom.png"))]
    (on-action button
      (doseq [chart (:charts @editor)]
	(doto (:chart-panel @chart)
	  (.setMouseZoomable true)
	  (.setFillZoomRectangle false))))
    button))

(defn init-edit-button [editor]
  (let [button (JToggleButton. (ImageIcon. "resources/edit.png"))]
    (on-action button
      (doseq [chart (:charts @editor)]
	(let [chart-panel (:chart-panel @chart)]
	  (.setMouseZoomable chart-panel false))))
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

(defn open-curve-editor [lasfile curves]   
  (let [frame (init-frame lasfile curves)
	[index dirty-curves] (lasso/adjust-curves (map (comp lasso/deref-curve deref) curves))
	editor (ref {})
	charts (for [[c d] (tuplize curves dirty-curves)]
		 (chart-controller/init-chart c d))
	depth-data (:data index)
	slider-notches 200
	slider (slider-controller/init-slider slider-notches)
	table (table-controller/init-table index dirty-curves)
	editor-props (struct-map Editor
		       :frame frame
		       :lasfile lasfile
		       :index index 
		       :slider slider
		       :table table
		       :charts charts)
	saveb (init-save-button editor)
	mergeb (init-merge-button editor)
	tool-bar (init-toolbar editor)
	tool-panel (init-tool-panel slider table saveb mergeb)
	main-panel (init-main-panel charts tool-panel tool-bar)]
    (dosync (ref-set editor editor-props))
    (add-watcher slider :send slider-watcher (partial scroll-table-and-chart editor))
    (add-watcher table :send table-watcher (partial sync-chart-with-table editor))
    (doseq [[chart col] (tuplize charts (range 1 (inc (count charts))))]
      (add-watcher chart :send chart-watcher (partial sync-table-with-chart editor col)))
    (swing
     (table-controller/show-percentage (:widget @table) 0)
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    editor))