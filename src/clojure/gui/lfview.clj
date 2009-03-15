(ns gui.lfview
  (:use util gui.ceditor
	gui.util gui.clipboard))

(import '(gui IconListCellRenderer ChartUtil)
	'(java.io File)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel SwingUtilities
		      JTabbedPane)
	'(javax.imageio ImageIO)
	'(net.miginfocom.swing MigLayout)
	'(java.awt Dimension Image)
	'(java.awt.event MouseMotionAdapter MouseAdapter MouseEvent))

(def stored-curves (ref {}))

(def image-processor (agent nil))

(def lfview-panel (new JTabbedPane))

(defn- directory-to-icons [path]
  (let [directory (new File path)
	files (.listFiles directory)]
    (for [file files]
      (let [image (ImageIO/read file)
	    scaled (.getScaledInstance image 64 64 Image/SCALE_SMOOTH)
	    icon (new ImageIcon scaled)
	    name (.getName file)]
	(new JLabel name icon JLabel/LEFT)))))

(defn- selected-curves [curve-list]
  (let [curves (get @stored-curves curve-list)
	find-curve (fn [name] (find-first #(= name (.getMnemonic %)) curves))]
    (map (comp find-curve #(.getText %))
	 (.getSelectedValues curve-list))))

(defn- open-curves-context-menu [event curve-list]
  (let [[c x y] [(.getComponent event) (.getX event) (.getY event)]
	scurves (selected-curves curve-list)]
    (context-menu [c x y]
      ["Copy" (fn [e] (dosync (ref-set copied-curves scurves)))]
      ["Paste" (fn [e] 
		 (let [cmodel (.getModel curve-list)]
		   (doseq [curve @copied-curves]
		     (.addElement cmodel (ChartUtil/curveToIcon curve)))
		   (dosync 
		    (let [curves (get @stored-curves curve-list)]
		      (alter stored-curves assoc curve-list 
			     (concat curves @copied-curves))))))])))

(defn open-lfview [lasfile]
  (swing 
   (let [curves (.getCurves lasfile)
	 cmodel (new DefaultListModel)
	 clist (new JList cmodel)
	 inner-panel (new JPanel (new MigLayout))
	 pane (new JScrollPane inner-panel)
	 outer-panel (new JPanel (new MigLayout))
	 editb (new JButton "Edit")]

     (doto clist
       (.setCellRenderer (new IconListCellRenderer))
       (.addMouseListener 
	(proxy [MouseAdapter] []
	  (mouseClicked [e]
			(when (= MouseEvent/BUTTON3 (.getButton e))
			  (open-curves-context-menu e clist))))))

     (send image-processor 
	   (fn [s]
	     (println "in image processor")
	     (let [icons (map #(ChartUtil/curveToIcon %) curves)]
	       (doseq [icon icons]
		 (swing 
		  (.addElement cmodel icon)
		  (.repaint clist))))
	     s))


     (dosync (alter stored-curves assoc clist curves))

     (.add inner-panel clist "pushx, pushy, growx, growy, wrap")

     (doto outer-panel 
       (.add pane "pushx, pushy, growx, growy, wrap")
       (.setPreferredSize (new Dimension 400 700))
       (.add editb))

     (on-action editb 
       (doseq [sc (selected-curves clist)]
	 (swing (open-curve-editor sc))))

     (.addTab lfview-panel "foo" outer-panel))))

