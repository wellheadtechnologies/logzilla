(ns gui.lfview
  (:use util gui.ceditor gui.util las))

(import '(gui IconListCellRenderer)
	'(java.io File)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel)
	'(javax.imageio ImageIO)
	'(java.awt Dimension Image))

(defn- directory-to-icons [path]
  (let [directory (new File path)
	files (.listFiles directory)]
    (for [file files]
      (let [image (ImageIO/read file)
	    scaled (.getScaledInstance image 64 64 Image/SCALE_SMOOTH)
	    icon (new ImageIcon scaled)
	    name (.getName file)]
	(new JLabel name icon JLabel/LEFT)))))

(defn las-file-view [lasfile]
  (let [curves (:las-curves lasfile)
	index (first curves)
	cmodel (new DefaultListModel)
	clist (new JList cmodel)
	panel (new JPanel)
	pane (new JScrollPane panel)
	editb (new JButton "Edit")]
    (doseq [curve curves]
      (.addElement cmodel (:mnemonic curve)))
    (.add panel clist)
    (.add panel editb)
    (on-action editb 
	       (let [scurves (map #(get-curve lasfile %) (.getSelectedValues clist))]
		 (doseq [sc scurves]
		   (open-curve-editor sc index))))
    pane))
	
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