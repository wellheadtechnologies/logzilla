(ns format.controller
  (:require slider.controller chart.controller chart.render
	    registry)
  (:use util global gutil messages lasso)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane JToolBar JButton JToggleButton 
			ButtonGroup ImageIcon JPanel TransferHandler
			ScrollPaneConstants JLabel
			JSlider JTable JToggleButton ImageIcon)
	   (java.awt Dimension Color Toolkit Image Point)
	   (javax.imageio ImageIO)
	   (java.io File)
	   (org.jfree.data Range)
	   (org.jfree.chart ChartMouseListener)
	   (net.miginfocom.swing MigLayout)))

(defstruct Formatter 
  :charts
  :slider
  :frame
  :canonical-percentage)

(defn create-frame []
  (let [name "Format Log"
	frame (registry/acquire-registered-frame)]
    (doto frame
      (.setTitle name))))

(defn create-export-button [formatter]
  (let [button (JButton. "Export")]
    (doto button
      (on-action 
       (let [chart (first (:charts @formatter))]
	 (chart.render/export-chart-to-pdf chart 400 300 "export.pdf"))))))

(defn create-main-panel [export-button slider-widget left-chart middle-chart right-chart]
  (let [panel (JPanel. (MigLayout. "ins 0, nogrid"))]
    (doto panel
      (.add export-button "wrap")
      (.add left-chart "push, grow")
      (.add slider-widget "pushy, growy")
      (.add middle-chart "push, grow")
      (.add right-chart "push, grow"))))

(defn update-canonical-percentage [formatter percentage]
  (dosync
   (let [{:keys [slider charts canonical-percentage]} @formatter]
     (when (not= canonical-percentage percentage)
       (alter formatter assoc :canonical-percentage percentage)
       (short-task
	(ignore :percentage-change slider (slider.controller/set-percentage slider percentage))
	(doseq [chart charts]
	  (ignore :percentage-change chart (chart.controller/show-percentage chart percentage))))))))

(defn remove-charts-from-panel [formatter panel]
  (swing-mutator
   (let [charts (:charts @formatter)]
     (doseq [chart charts]
       (let [chart-panel (:chart-panel @chart)
	     components (.getComponents panel)]
	 (when (some #(= chart-panel %) components)
	   (doto panel
	     (.remove chart-panel)
	     (.revalidate)
	     (.repaint))
	   (dosync 
	    (alter formatter assoc :charts (remove #(= chart %) (:charts @formatter))))))))))

(defn add-chart [formatter panel curve]
  (let [chart (chart.controller/init-chart curve (deref-curve @curve))
	chart-panel (:chart-panel @chart)
	old-charts (:charts @formatter)]
    (dosync 
     (alter formatter assoc :charts (conj old-charts chart))
     (swing-agent
      (remove-charts-from-panel formatter panel)
      (let [components (.getComponents panel)]
	(doto panel
	  (.removeAll)
	  (.add chart-panel "push, grow")
	  (.revalidate)
	  (.repaint)
	  (ignore :percentage-change chart (chart.controller/show-percentage chart (:canonical-percentage @formatter)))))))))

(defn curve-transfer-handler [formatter]
  (proxy [TransferHandler] []
    (canImport [comp transfer-flavors] true)
    (importData [comp t]
		(try
		 (let [curve (.getCurve (.getTransferData t ref-data-flavor))]
		   (add-chart formatter comp curve))
		 (catch Exception e
		   (.printStackTrace e)
		   (throw e))))))

(defn create-empty-panel [formatter]
  (doto (JPanel. (MigLayout. "ins 0"))
    (.setTransferHandler (curve-transfer-handler formatter))
    (.add (JLabel. "Drag Curve Here!"))
    (.setBackground Color/white)
    (.setPreferredSize (Dimension. 400 700))))

(defn open-formatter []
  (let [formatter (ref nil)
	frame (create-frame)
	export-button (create-export-button formatter)
	slider (slider.controller/init-slider 200)
	left-chart (create-empty-panel formatter)
	middle-chart (create-empty-panel formatter)
	right-chart (create-empty-panel formatter)
	main-panel (create-main-panel export-button (:widget @slider) left-chart middle-chart right-chart)
	f (struct-map Formatter
	    :charts []
	    :slider slider
	    :frame frame)]
    (dosync (ref-set formatter f))

    (add-listener :percentage-change slider formatter
		  (fn [event]
		    (update-canonical-percentage formatter (:percentage event))))

    (swing-agent
     (update-canonical-percentage formatter 0)
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))))
