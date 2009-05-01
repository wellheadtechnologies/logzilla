(ns format.controller
  (:require slider.controller)
  (:use util global gutil messages)
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

(defn curve-transfer-handler []
  (proxy [TransferHandler] []
    (canImport [comp transfer-flavors] true)
    (importData [comp t] (println "you dropped something!"))))

(defn create-place-holder []
  (doto (JPanel.)
    (.setTransferHandler (curve-transfer-handler))
    (.add (JLabel. "Drag Curve Here!"))
    (.setBackground Color/white)
    (.setPreferredSize (Dimension. 400 700))))

(defn open-formatter []
  (let [frame (create-frame)
	slider (slider.controller/init-slider 200)
	left-chart (create-place-holder)
	middle-chart (create-place-holder)
	right-chart (create-place-holder)
	main-panel (create-main-panel (:widget @slider) left-chart middle-chart right-chart)]
    (swing
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))))