(ns merger.controller
  (:require chart.controller lasso)
  (:use util gutil)
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

(defn init-frame [lasfile curves]
  (let [name (apply str (map #(str (get-in (deref %) [:descriptor :mnemonic]) "  ") curves))
	frame (JFrame. (str "Merge " (:name @lasfile) " | " name))]
    frame))

(defn init-main-panel [left-panel charts]
  (let [main-panel (JPanel. (MigLayout. "ins 0"))
	width (* (count charts) 500)]
    (doseq [chart charts]
      (doto main-panel
	(.add left-panel "pushy, growy")
	(.add (:chart-panel @chart) "push, grow")))
    (doto main-panel
      (.setPreferredSize (Dimension. width 900)))))

(defn init-left-panel [lasfile curves]
  (let [panel (JPanel. (MigLayout. "ins 0"))]
    panel))

(defn open-curve-merger [lasfile curves]
  (let [frame (init-frame lasfile curves)
	dirty-curves (doall (map #(lasso/deref-curve (deref %)) curves))
	charts (doall (for [[c d] (tuplize curves dirty-curves)] (chart.controller/init-chart c d)))
	left-panel (init-left-panel lasfile curves)
	main-panel (init-main-panel left-panel charts)]
    (doto frame
      (.add main-panel)
      (.pack)
      (.setVisible true))))