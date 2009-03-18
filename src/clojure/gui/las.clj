(ns gui.las
  (:use util gui.curves gui.util gui.global gui.widgets)
  (:import (gui IconListCellRenderer ChartUtil)
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

(def las-views (agent {}))
(def current-las-view (agent (create-curve-panel)))
(def las-panel 
     (let [panel (create-titled-panel "Curves")]
       (doto panel
	 (.add @current-las-view "pushy, growy, pushx, growx")
	 (.revalidate))
       panel))

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

(defn insert-las-view [lasfile view]
  (send las-views #(assoc % lasfile view)))

(defn add-curve [jlist curve]
  (let [icon (ChartUtil/curveToIcon curve)]
    (swing (.addElement (.getModel jlist) icon))))

(defn- get-selected-curves [jlist curves]
  (let [selected (map #(.getText %) (.getSelectedValues jlist))]
    (filter (fn [curve]
	      (let [name (.getMnemonic curve)]
		(some #(= name %) selected)))
	    curves)))

(defn- open-curves-context-menu [event jlist curves]
  (let [[c x y] [(.getComponent event) (.getX event) (.getY event)]
	scurves (get-selected-curves jlist curves)]
    (context-menu [c x y]
      ["Copy" (fn [e] (send copied-curves (fn [x] scurves)))]
      ["Paste" (fn [e] 
		 (doseq [curve @copied-curves]
		   (add-curve jlist curve)))])))

(defn- create-curve-list [curves]
  (let [jlist (create-jlist)]
    (long-task
     (doseq [curve curves]
       (add-curve jlist curve)))

    (swing 
     (doto jlist
       (.setFixedCellHeight 80)
       (.setOpaque false))
     
     (on-click jlist
       (fn [e]
	 (cond
	  (and (= MouseEvent/BUTTON1 (.getButton e))
	       (= 2 (.getClickCount e)))
	  (doseq [sc (get-selected-curves jlist curves)]
	    (swing (open-curve-editor sc)))
	  
	  (= MouseEvent/BUTTON3 (.getButton e))
	  (swing (open-curves-context-menu e jlist))))))
    jlist))

(defn create-las-view [lasfile]
  (swing 
   (let [curves (.getCurves lasfile)
	 curve-list (create-curve-list curves)
	 inner-panel (create-inner-panel)
	 pane (new JScrollPane inner-panel)
	 outer-panel (create-curve-panel)]

     (doto inner-panel
       (.add curve-list "pushx, growx, pushy, growy, wrap"))
     (doto outer-panel 
       (.add pane "pushx, pushy, growx, growy, wrap")
       (.setPreferredSize (new Dimension 400 700)))
     outer-panel)))

(defn open-las-view [lasfile]
  (swing
   (set-las-view 
    (let [existing-view (get @las-views lasfile)]
      (if existing-view
	existing-view
	(let [view (create-las-view lasfile)]
	  (insert-las-view lasfile view)
	  view))))))