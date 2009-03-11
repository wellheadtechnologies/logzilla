(ns gui.lfview
  (:use util gui.ceditor
	gui.util las
	gui.clipboard))

(import '(gui IconListCellRenderer)
	'(java.io File)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel)
	'(javax.imageio ImageIO)
	'(net.miginfocom.swing MigLayout)
	'(java.awt Dimension Image)
	'(java.awt.event MouseMotionAdapter MouseAdapter MouseEvent))

(defn- directory-to-icons [path]
  (let [directory (new File path)
	files (.listFiles directory)]
    (for [file files]
      (let [image (ImageIO/read file)
	    scaled (.getScaledInstance image 64 64 Image/SCALE_SMOOTH)
	    icon (new ImageIcon scaled)
	    name (.getName file)]
	(new JLabel name icon JLabel/LEFT)))))

(defn- curve-to-icon [curve index]
  (let [image (curve-to-image curve index)
	icon (new ImageIcon image)
	name (:mnemonic curve)]
    (new JLabel name icon JLabel/LEFT)))

(defn- selected-curves [lasfile curve-list]
  (map (comp (partial get-curve lasfile) #(.getText %))
       (.getSelectedValues curve-list)))

(defn- open-curves-context-menu [event lasfile curve-list]
  (let [[c x y] [(.getComponent event) (.getX event) (.getY event)]
	scurves (selected-curves lasfile curve-list)]
    (context-menu [c x y]
      ["Copy" (fn [e] (dosync (ref-set copied-curves scurves)))]
      ["Paste" (fn [e] 
		 (let [cmodel (.getModel curve-list)]
		   (doseq [curve @copied-curves]
		     (add-curve lasfile curve)
		     (.addElement cmodel (curve-to-icon curve index)))))])))    

(defn las-file-view [lasfile]
  (let [_curves (:las-curves lasfile)
	index (first _curves)
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
			 (open-curves-context-menu e lasfile clist))))))
       
    (doseq [curve curves]
      (.addElement cmodel (curve-to-icon curve index)))

    (.add inner-panel clist "pushx, pushy, growx, growy, wrap")

    (doto outer-panel 
      (.add pane "pushx, pushy, growx, growy, wrap")
      (.add editb))

    (on-action editb 
      (doseq [sc (selected-curves lasfile clist)]
	(open-curve-editor sc index)))

    outer-panel))
	
(defn demo []
  (let [frame (new JFrame "Curve View")
	pane (new JScrollPane)
	model (new DefaultListModel)
	list (new JList model)
	icons (directory-to-icons "images")]

    (doseq [icon icons]
      (.addElement model icon))

    (doto list
      (.setCellRenderer (new IconListCellRenderer))
      (.setLayoutOrientation JList/HORIZONTAL_WRAP))

    (.. pane (getViewport) (setView list))
    (.. frame (getContentPane) (add pane))

    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.pack)
      (.setVisible true))))