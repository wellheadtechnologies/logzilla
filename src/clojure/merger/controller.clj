(ns merger.controller
  (:require chart.controller lasso slider.controller)
  (:use util gutil global messages)
  (:import (javax.swing.event TableModelListener ChangeListener)
	   (javax.swing JFrame JScrollPane JToolBar JButton JToggleButton 
			ButtonGroup ImageIcon JPanel
			ScrollPaneConstants)
	   (java.awt Dimension Color Toolkit Image Point)
	   (javax.imageio ImageIO)
	   (java.io File)
	   (gui ChartUtil)
	   (org.jfree.data Range)
	   (org.jfree.chart ChartMouseListener)
	   (net.miginfocom.swing MigLayout)))

(defstruct Merger
  :frame
  :lasfile 
  :index
  :slider
  :charts
  :width
  :height
  :canonical-percentage)

(defn merge-curves [left right]
  {:descriptor (:descriptor @left)
   :index (:index @left)
   :data (ChartUtil/mergeData (:data @left) (:data @right))
   })

(defn zero-out [curve]
  (assoc curve 
    :data (map (fn [_] Double/NaN) (:data curve))))

(def slider-notches 200)

(defn update-canonical-percentage [merger percentage]
  (dosync
   (let [{:keys [slider charts canonical-percentage result-chart]} @merger]
     (when (not= canonical-percentage percentage)
       (alter merger assoc :canonical-percentage percentage)
       (short-task
	(ignore :percentage-change slider (slider.controller/set-percentage slider percentage))
	(ignore :percentage-change result-chart (chart.controller/show-percentage result-chart percentage))
	(doseq [chart charts]
	  (ignore :percentage-change chart (chart.controller/show-percentage chart percentage))))))))

(defn init-frame [lasfile curves]
  (let [name (apply str (map #(str (get-in (deref %) [:descriptor :mnemonic]) "  ") curves))
	frame (JFrame. (str "Merge " (:name @lasfile) " | " name))]
    frame))

(defn init-main-panel [left-panel toolbar result-chart charts]
  (let [main-panel (JPanel. (MigLayout. "ins 0, nogrid"))
	chart-count (inc (count charts))
	width (* chart-count 500)
	charts-panel (JPanel. (MigLayout. "ins 0"))]
    (doseq [chart charts]
      (doto charts-panel
	(.add (:chart-panel @chart) "pushy, growy, width 500px, growx")))
    (doto main-panel
      (.setPreferredSize (Dimension. width 900))
      (.add toolbar "pushx, growx, wrap")
      (.add (:chart-panel @result-chart) "pushy, growy, width 500px, growx")
      (.add left-panel "pushy, growy")
      (.add charts-panel "push, grow"))))

(defn init-left-panel [slider]
  (let [panel (JPanel. (MigLayout. "ins 0"))]
    (doto panel 
      (.add (:widget @slider) "pushy, growy"))))

(defn init-merge-button [merger]
  (let [button (JButton. "Merge")]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action 
       (let [charts (:charts @merger)
	     result-chart (:result-chart @merger)
	     merged-curve (merge-curves (only (:curves @(first charts))) (only (:curves @(second charts))))]
	 (swing-agent
	  (chart.controller/update-curve result-chart 0 (lasso/deref-curve merged-curve))))))))

(defn init-tool-bar [merger]
  (let [toolbar (JToolBar. JToolBar/HORIZONTAL)
	merge-button (init-merge-button merger)]
    (doto toolbar
      (.setFloatable false)
      (.add merge-button))))

(defn init-result-chart [index]
  (let [dcurve (assoc (zero-out index) 
		 :index index
		 :descriptor {:mnemonic "Result"
			      :unit ""
			      :data ""
			      :description "Result of a Merge"})
	curve (ref dcurve)
	chart (chart.controller/init-chart curve dcurve)]
    (println "descriptor = " (:descriptor dcurve))
    chart))

(defn open-curve-merger [lasfile curves]
  (let [frame (init-frame lasfile curves)
	merger (ref {})
	dirty-curves (doall (map #(lasso/deref-curve (deref %)) curves))
	[index dirty-curves] (lasso/adjust-curves dirty-curves)
	charts (doall (for [[c d] (tuplize curves dirty-curves)] (chart.controller/init-chart c d)))
	result-chart (init-result-chart index)
	slider (slider.controller/init-slider slider-notches)
	left-panel (init-left-panel slider)
	toolbar (init-tool-bar merger)
	main-panel (init-main-panel left-panel toolbar result-chart charts)
	merger-props (struct-map Merger
		       :frame frame
		       :lasfile lasfile
		       :index index
		       :slider slider
		       :charts charts
		       :result-chart result-chart)]
    (dosync (ref-set merger merger-props))
    (add-listener :percentage-change slider merger
		  (fn [event]
		    (update-canonical-percentage merger (:percentage event))))
    (swing-agent
     (update-canonical-percentage merger 0)
     (doto frame
       (.add main-panel)
       (.pack)
       (.setVisible true)))
    merger))
