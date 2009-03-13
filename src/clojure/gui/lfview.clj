(ns gui.lfview
  (:use util gui.ceditor
	gui.util las
	gui.clipboard))

(import '(gui IconListCellRenderer)
	'(java.io File)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel SwingUtilities)
	'(javax.imageio ImageIO)
	'(net.miginfocom.swing MigLayout)
	'(java.awt Dimension Image)
	'(java.awt.event MouseMotionAdapter MouseAdapter MouseEvent))

(def stored-curves (ref {}))

(def image-processor (agent nil))

(defn- directory-to-icons [path]
  (let [directory (new File path)
	files (.listFiles directory)]
    (for [file files]
      (let [image (ImageIO/read file)
	    scaled (.getScaledInstance image 64 64 Image/SCALE_SMOOTH)
	    icon (new ImageIcon scaled)
	    name (.getName file)]
	(new JLabel name icon JLabel/LEFT)))))

(defn- curve-to-icon [curve]
  (let [image (curve-to-image curve)
	icon (new ImageIcon image)
	name (:mnemonic curve)]
    (new JLabel name icon JLabel/LEFT)))

(defn- selected-curves [curve-list]
  (let [curves (get @stored-curves curve-list)]
    (map (comp #(find-curve curves %) #(.getText %))
	 (.getSelectedValues curve-list))))

(defn- open-curves-context-menu [event curve-list]
  (let [[c x y] [(.getComponent event) (.getX event) (.getY event)]
	scurves (selected-curves curve-list)]
    (context-menu [c x y]
      ["Copy" (fn [e] (dosync (ref-set copied-curves scurves)))]
      ["Paste" (fn [e] 
		 (let [cmodel (.getModel curve-list)]
		   (doseq [curve @copied-curves]
		     (.addElement cmodel (curve-to-icon curve)))
		   (dosync 
		    (let [curves (get @stored-curves curve-list)]
		      (alter stored-curves assoc curve-list 
			     (concat curves @copied-curves))))))])))

(defn las-file-view [lasfile]
  (let [_curves (:curves lasfile)
	curves (rest _curves)
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
	    (let [icons (map curve-to-icon curves)]
	      (doseq [icon icons]
		(SwingUtilities/invokeLater 
		 (fn [] (.addElement cmodel icon))))
	      s)))

    (dosync (alter stored-curves assoc clist curves))

    (.add inner-panel clist "pushx, pushy, growx, growy, wrap")

    (doto outer-panel 
      (.add pane "pushx, pushy, growx, growy, wrap")
      (.setPreferredSize (new Dimension 400 700))
      (.add editb))

    (on-action editb 
      (doseq [sc (selected-curves clist)]
	(open-curve-editor sc)))

    outer-panel))
