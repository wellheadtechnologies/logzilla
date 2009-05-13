(ns editor.controller
  (:require lasso
	    [slider.controller :as slider-controller]
	    [editor.table.controller :as table-controller]
	    [chart.controller :as chart-controller])
  (:use util global gutil chart.controller messages)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane JToolBar JButton JToggleButton 
			ButtonGroup ImageIcon JPanel
			ScrollPaneConstants
			JSlider JTable JToggleButton ImageIcon)
	   (java.awt Dimension Color Toolkit Image Point)
	   (javax.imageio ImageIO)
	   (java.io File)
	   (org.jfree.data Range)
	   (org.jfree.chart ChartMouseListener)
	   (net.miginfocom.swing MigLayout)))

(defstruct Editor
  :frame 
  :lasfile
  :index
  :slider
  :table
  :chart
  :width
  :height)

(defn index-to-row [table index] index)

(defn row-to-index [table row] row)

(defn convert-to-double [value]
  (if (string? value)
    (Double/valueOf value)
    (double value)))

(defn not-dragging-anything [editor]
  (dosync
   (let [chart (:chart @editor)
	 dragged-entity (:dragged-entity @chart)]
     (nil? dragged-entity))))

(defn save [editor]
  (dosync
   (let [chart (:chart @editor)]
     (chart-controller/save-chart chart))))

(defn create-main-panel [chart table-panel left-toolbar right-toolbar]
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

(defmulti update-canonical-percentage (fn [editor x] 
					(cond 
					 (= (class x) clojure.lang.PersistentArrayMap) :event
					 (number? x) :percentage)))

(defmethod update-canonical-percentage :percentage [editor percentage]
  (update-canonical-percentage editor {:source editor :percentage percentage}))

(defmethod update-canonical-percentage :event [editor event]
  (let [{:keys [percentage source]} event
	{:keys [slider table chart canonical-percentage]} @editor]
    (when (not= canonical-percentage percentage)
      (short-task 
       (receive :percentage-change slider event)
       (receive :percentage-change table event)
       (receive :percentage-change chart event)))))

(defn update-table [table event]
  (let [{:keys [row value]} event
	table-widget (:widget @table)]
    (swing-agent
     (let [model (.getModel table-widget)]
       (ignore :value-change table (.setValueAt model value row 1))
       (table-controller/show-cell table-widget row 1)))))

(defn update-chart [chart event]
  (let [{:keys [index value]} event]
    (swing-agent
     (let [value (convert-to-double value)]
       (ignore :value-change chart 
	       (chart-controller/set-chart-value chart 0 index value)
	       (chart-controller/save-chart chart))))))

(defn create-save-button [editor]
  (let [button (JButton. (ImageIcon. "resources/save.png"))]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action 
       (save editor)))))

(defn create-edit-button [editor]
  (let [button (JToggleButton. (ImageIcon. "resources/edit.png"))]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action
       (let [chart (:chart @editor)]
	 (dosync
	  (disable-panning chart)
	  (disable-zooming chart)
	  (enable-dragging chart)))))))

(defn create-zoom-button [editor]
  (let [button (JToggleButton. (ImageIcon. "resources/zoom.png"))]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action 
       (let [chart (:chart @editor)]
	 (dosync
	  (disable-panning chart)
	  (disable-dragging chart)
	  (enable-zooming chart)))))))

(defn create-reset-button [editor]
  (let [button (JButton. "Reset Scale")]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action
       (let [chart (:chart @editor)]
	 (reset chart))))))

(defn create-points-button [editor]
  (let [button (JToggleButton. "Points")]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action
       (let [chart (:chart @editor)]
	 (toggle-points chart))))))

(defn create-pan-button [editor]
  (let [button (JToggleButton. (ImageIcon. glove-image))]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action
       (let [chart (:chart @editor)]
	 (dosync
	  (disable-zooming chart)
	  (disable-dragging chart)
	  (toggle-panning chart)))))))

(defn create-frame [lasfile curve]
  (let [name (get-in @curve [:descriptor :mnemonic])
	frame (JFrame. (str (:name @lasfile) " " name))]
    frame))

(defn create-table-panel [slider table]
  (let [panel (JPanel. (MigLayout. "ins 0"))]
    (doto panel
      (.add (:widget @slider) "pushy, growy")
      (.add (:pane @table) "push, grow"))))

(defn create-right-toolbar [editor]
  (let [toolbar (JToolBar. JToolBar/HORIZONTAL)
	zoom-button (create-zoom-button editor)
	edit-button (create-edit-button editor)
	reset-button (create-reset-button editor)
	points-button (create-points-button editor)
	pan-button (create-pan-button editor)
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

(defn create-left-toolbar [editor]
  (let [toolbar (JToolBar. JToolBar/HORIZONTAL)]
    (doto toolbar
      (.setFloatable false))))

(defn open-curve-editor [lasfile curve]   
  (let [frame (create-frame lasfile curve)
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
	left-tool-bar (create-left-toolbar editor)
	right-tool-bar (create-right-toolbar editor)
	table-panel (create-table-panel slider table)
	main-panel (create-main-panel chart table-panel left-tool-bar right-tool-bar)]

    (dosync (ref-set editor editor-props))
    (chart-controller/enable-dragging chart)

    (add-listener :percentage-change slider editor #(update-canonical-percentage editor %))
    (add-listener :percentage-change chart editor #(update-canonical-percentage editor %))
    (add-listener :value-change chart editor
		  (fn [event] 
		    (dosync
		     (let [{:keys [data-index value]} event
			   row (index-to-row (:widget @table) data-index)]
		       (update-table table {:row row :value value})))))
    (add-listener :value-change table editor
		  (fn [event]
		    (dosync
		     (let [{:keys [row value]} event
			   index (row-to-index (:widget @table) row)]
		       (update-chart chart {:index index :value value})
		       ))))

    (swing-agent
     (update-canonical-percentage editor 0)
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    editor))
