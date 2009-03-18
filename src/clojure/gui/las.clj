(ns gui.las
  (:use util gui.curves gui.util gui.global gui.widgets)
  (:import (gui IconListCellRenderer ChartUtil CurveList)
	   (java.io File)
	   (javax.swing JList JFrame DefaultListModel ImageIcon JLabel
			JScrollPane JButton JWindow JPanel SwingUtilities
			JTabbedPane BorderFactory)
	   (javax.swing.border BevelBorder)
	   (javax.imageio ImageIO)
	   (net.miginfocom.swing MigLayout)
	   (java.awt Dimension Image Color)
	   (java.awt.event MouseMotionAdapter MouseAdapter MouseEvent)))

(defn- create-curve-panel []
  (let [panel (new JPanel (new MigLayout))]
    (doto panel
      (.setBorder (BorderFactory/createEtchedBorder)))
    panel))

(defn- create-inner-panel []
     (let [panel (new JPanel (new MigLayout))]
       (doto panel
	 (.setBorder (BorderFactory/createEmptyBorder)))
       panel))

(def current-las-view (agent (create-curve-panel)))

(def las-panel 
     (let [panel (create-titled-panel "Curves")]
       (doto panel
	 (.add @current-las-view "pushy, growy, pushx, growx")
	 (.revalidate))
       panel))

(def las-views (agent {}))

(defn- open-curves-context-menu [event curve-list]
  (let [[c x y] [(.getComponent event) (.getX event) (.getY event)]
	scurves (.getSelectedCurves curve-list)]
    (context-menu [c x y]
      ["Copy" (fn [e] (send copied-curves (fn [x] scurves)))]
      ["Paste" (fn [e] 
		 (doseq [curve @copied-curves]
		   (.addCurve curve-list curve)))])))

(defn set-las-view [new-view]
  (send current-las-view
	(fn [old-view]
	  (swing 
	   (doto las-panel
	     (.remove old-view)
	     (.add new-view)
	     (.revalidate))
	   (doto new-view
	     (.repaint)
	     (.revalidate)))
	  new-view)))

(defn create-las-view [lasfile]
  (swing 
   (let [curves (.getCurves lasfile)
	 curve-list (new CurveList)
	 inner-panel (create-inner-panel)
	 pane (new JScrollPane inner-panel)
	 outer-panel (create-curve-panel)]

     (long-task (.addCurves curve-list curves))

     (doto curve-list
       (.setFixedCellHeight 80)
       (.setOpaque false))

     (on-click curve-list
       (fn [e]
	 (cond
	  (and (= MouseEvent/BUTTON1 (.getButton e))
	       (= 2 (.getClickCount e)))
	  (doseq [sc (.getSelectedCurves curve-list)]
	    (swing (open-curve-editor sc)))
	 
	  (= MouseEvent/BUTTON3 (.getButton e))
	  (swing (open-curves-context-menu e curve-list)))))
     
     (doto inner-panel
       (.add curve-list "pushx, growx, pushy, growy, wrap"))
     (doto outer-panel 
       (.add pane "pushx, pushy, growx, growy, wrap")
       (.setPreferredSize (new Dimension 400 700)))
     outer-panel)))

(defn insert-las-view [lasfile view]
  (println "inserting " (.getName lasfile))
  (send las-views #(assoc % lasfile view)))

(defn open-las-view [lasfile]
  (swing
   (set-las-view 
    (let [existing-view (get @las-views lasfile)]
      (if existing-view
	existing-view
	(let [view (create-las-view lasfile)]
	  (insert-las-view lasfile view)
	  view))))))