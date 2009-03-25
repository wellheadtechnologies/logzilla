(ns gui.widgets)
(import '(javax.swing JPanel JLabel JFileChooser 
		      JMenu JPopupMenu SwingUtilities
		      JList DefaultListModel JButton
		      JTabbedPane BorderFactory)
	'(java.awt Dimension)
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

(defn panel [& widget-layouts]
  (let [panel (new JPanel (new MigLayout))]
    (doseq [[widget layout] widget-layouts]
      (.add panel widget layout))
    panel))

(defn panelS [width height & widget-layouts]
  (let [p (apply panel widget-layouts)]
    (.setPreferredSize p (new Dimension width height))
    p))

(defn button [name fun]
  (let [b (new JButton name)]
    (.addActionListener b
     (proxy [ActionListener] []
       (actionPerformed [e] (fun e))))
    b))
