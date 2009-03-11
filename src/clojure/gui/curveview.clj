(ns gui.curveview
  (:use util))

(import '(gui IconListCellRenderer)
	'(java.io File)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane)
	'(javax.imageio ImageIO)
	'(java.awt Dimension Image))

(defn directory-to-icons [path]
  (let [directory (new File path)
	files (.listFiles directory)]
    (for [file files]
      (let [image (ImageIO/read file)
	    scaled (.getScaledInstance image 64 64 Image/SCALE_SMOOTH)
	    icon (new ImageIcon scaled)
	    name (.getName file)]
	(new JLabel name icon JLabel/LEFT)))))
	

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
    (.setVisible true)))
