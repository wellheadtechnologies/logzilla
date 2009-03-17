(ns gui.lfview
  (:use util gui.ceditor
	gui.util gui.global gui.widgets))

(import '(gui IconListCellRenderer ChartUtil CurveList)
	'(java.io File)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel SwingUtilities
		      JTabbedPane BorderFactory)
	'(javax.swing.border BevelBorder)
	'(javax.imageio ImageIO)
	'(net.miginfocom.swing MigLayout)
	'(java.awt Dimension Image Color)
	'(java.awt.event MouseMotionAdapter MouseAdapter MouseEvent))

(defn create-curve-panel []
  (let [panel (new JPanel (new MigLayout))]
    (doto panel
      (.setBorder (BorderFactory/createEtchedBorder)))
    panel))

(defn create-inner-panel []
  (let [panel (new JPanel (new MigLayout))]
    (doto panel
      (.setBorder (BorderFactory/createEmptyBorder)))
    panel))

(def current-curve-view 
     (agent (create-curve-panel)))

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

(defn install-curve-view [lasfile]
  (swing 
   (let [curves (.getCurves lasfile)
	 curve-list (new CurveList)
	 inner-panel (create-inner-panel)
	 pane (new JScrollPane inner-panel)
	 outer-panel (create-curve-panel)]

     (long-task (.addCurves curve-list curves))

     (doto curve-list
       (.setFixedCellHeight 80))

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
       (.add curve-list "pushx, growx, wrap"))
     (doto outer-panel 
       (.add pane "pushx, pushy, growx, growy, wrap")
       (.setPreferredSize (new Dimension 400 700)))
     (dosync (alter curve-views assoc lasfile outer-panel))
     outer-panel)))

(defn open-curve-view [lasfile]
  (swing
   (set-curve-view 
    (let [existing-view (get @curve-views lasfile)]
      (if existing-view
	existing-view
	(install-curve-view))))))