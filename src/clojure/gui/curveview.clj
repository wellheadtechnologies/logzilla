(import '(gui IconListCellRenderer)
	'(javax.swing JList JFrame DefaultListModel ImageIcon)
	'(java.awt Dimension))

(let [frame (new JFrame "Curve View")
      list-model (new DefaultListModel)
      curves (new JList list-model)]
  (doto list-model
    (.add 0 (new JLabel "one" (new ImageIcon "images/facies.png"))))
  (doto curves 
    (.setCellRenderer (new IconListCellRenderer)))
  (.. frame (getContentPane) (add curves))
  (doto frame
    (.pack)
    (.setVisible true)))
  