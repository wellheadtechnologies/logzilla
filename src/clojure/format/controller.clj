(ns format.controller
  (:require slider.controller chart.controller)
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
	frame (JFrame. name)]
    frame))

(defn create-main-panel [slider-widget left-chart middle-chart right-chart]
  (let [panel (JPanel. (MigLayout. "ins 0"))]
    (doto panel
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

(defn add-chart [formatter panel curve]
  (let [chart (chart.controller/init-chart curve (deref-curve @curve))
	chart-panel (:chart-panel @chart)
	old-charts (:charts @formatter)]
    (dosync 
     (alter formatter assoc :charts (conj old-charts chart))
     (swing
      (doto panel
	(.removeAll)
	(.add chart-panel "push, grow")
	(.revalidate)
	(.repaint)
	(ignore :percentage-change chart (chart.controller/show-percentage chart (:canonical-percentage @formatter))))))))

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
	slider (slider.controller/init-slider 200)
	left-chart (create-empty-panel formatter)
	middle-chart (create-empty-panel formatter)
	right-chart (create-empty-panel formatter)
	main-panel (create-main-panel (:widget @slider) left-chart middle-chart right-chart)
	f (struct-map Formatter
	    :charts []
	    :slider slider
	    :frame frame)]
    (dosync (ref-set formatter f))

    (add-listener :percentage-change slider
		  (fn [event]
		    (update-canonical-percentage formatter (:percentage event))))

    (swing
     (update-canonical-percentage formatter 0)
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))))