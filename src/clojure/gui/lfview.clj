(ns gui.lfview
  (:use util gui.ceditor
	gui.util gui.global gui.widgets))

(import '(gui IconListCellRenderer ChartUtil CurveList)
	'(java.io File)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel SwingUtilities
		      JTabbedPane)
	'(javax.imageio ImageIO)
	'(net.miginfocom.swing MigLayout)
	'(java.awt Dimension Image Color)
	'(java.awt.event MouseMotionAdapter MouseAdapter MouseEvent))

(def current-curve-view 
     (agent 
      (let [panel (new JPanel)]
	(doto panel
	  (.setBackground Color/white))
	panel)))

(def curve-panel (let [panel (create-titled-panel "Curves")]
		   (doto panel
		     (.add @current-curve-view "pushy, growy, pushx, growx")
		     (.revalidate))
		   panel))

(def curve-views (ref {}))

(defn- open-curves-context-menu [event curve-list]
  (let [[c x y] [(.getComponent event) (.getX event) (.getY event)]
	scurves (.getSelectedCurves curve-list)]
    (context-menu [c x y]
      ["Copy" (fn [e] (send copied-curves (fn [x] scurves)))]
      ["Paste" (fn [e] 
		 (doseq [curve @copied-curves]
		   (.addCurve curve-list curve)))])))

(defn set-curve-view [new-view]
  (send current-curve-view
	(fn [old-view]
	  (swing 
	   (doto curve-panel
	     (.remove old-view)
	     (.add new-view)
	     (.revalidate))
	   (doto new-view
	     (.repaint)
	     (.revalidate)))
	  new-view)))

(defn open-curve-view [lasfile]
  (swing
   (set-curve-view 
    (let [existing-view (get @curve-views lasfile)]
      (if existing-view
	(do
	  (println "found preexisting lasfile named " (.getName lasfile))
	  existing-view)
	(let [curves (.getCurves lasfile)
	      curve-list (new CurveList)
	      inner-panel (new JPanel (new MigLayout))
	      pane (new JScrollPane inner-panel)
	      outer-panel (new JPanel (new MigLayout))]

	  (execute-later (.addCurves curve-list curves))
	  (.add inner-panel curve-list "pushx, pushy, growx, growy, wrap")
	
	  (doto outer-panel 
	    (.add pane "pushx, pushy, growx, growy, wrap")
	    (.setPreferredSize (new Dimension 400 700)))
	  (dosync (alter curve-views assoc lasfile outer-panel))
	  outer-panel))))))


;(defn open-lfview [lasfile]
;  (swing 
;   (let [curves (.getCurves lasfile)
;	 curve-list (new CurveList)
;	 inner-panel (new JPanel (new MigLayout))
;	 pane (new JScrollPane inner-panel)
;	 outer-panel (new JPanel (new MigLayout))
;	 editb (new JButton "Edit")]
;
;     (execute-later (.addCurves curve-list curves))
;
;     (on-click curve-list
;       (fn [e]
;	 (when (= MouseEvent/BUTTON3 (.getButton e))
;	   (open-curves-context-menu e curve-list))))
;
;     (.add inner-panel curve-list "pushx, pushy, growx, growy, wrap")
;
;     (doto outer-panel 
;       (.add pane "pushx, pushy, growx, growy, wrap")
;       (.setPreferredSize (new Dimension 400 700))
;       (.add editb))
;
;     (on-action editb 
;       (doseq [sc (.getSelectedCurves curve-list)]
;	 (swing (open-curve-editor sc))))
;
;     (.addTab lfview-panel (.getName lasfile) outer-panel))))
;
;
