(ns gui.widgets)
(import '(javax.swing JPanel JLabel JFileChooser 
		      JMenu JPopupMenu SwingUtilities
		      JList DefaultListModel
		      JTabbedPane BorderFactory)
	'(net.miginfocom.swing MigLayout)
	'(java.awt.event MouseAdapter ActionListener)
	'(gui IconListCellRenderer))

(defn create-titled-panel [title]
  (let [title-panel (new JPanel)
	outer-panel (new JPanel (new MigLayout))]
    (doto title-panel
      (.add (new JLabel title)))
    (doto outer-panel
      (.add title-panel "growx, wrap"))
    outer-panel))
    

(defn create-jlist []
  (let [model (new DefaultListModel)
	jlist (new JList model)
	renderer (new IconListCellRenderer)]
    (doto jlist
      (.setCellRenderer renderer)
      (.setModel model))
    jlist))

(defn create-inner-panel []
     (let [panel (new JPanel (new MigLayout))]
       (doto panel
	 (.setBorder (BorderFactory/createEmptyBorder)))
       panel))
