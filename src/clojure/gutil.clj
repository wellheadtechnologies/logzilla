(ns gutil
  (:import (java.awt.event ActionListener)
	   (javax.swing JPanel JLabel JButton
			JFileChooser JMenu JPopupMenu 
			SwingUtilities JList DefaultListModel
			JTabbedPane BorderFactory)
	   (java.awt Dimension)
	   (java.awt.event MouseAdapter)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))


(defn actions [menu & name-actions]
  (doseq [[name action] name-actions]
    (let [item (.add menu name)]
      (.addActionListener item (proxy [ActionListener] []
				 (actionPerformed [e] (action e)))))))

(defn menu [name & name-actions]
  (let [m (new JMenu name)]
    (apply actions name-actions)
    m))

(defn context-menu [[c x y] & name-actions]
  (let [m (new JPopupMenu)]
    (apply actions m name-actions)
    (.show m c x y)
    m))

(defmacro on-action [widget & body]
  `(.addActionListener ~widget 
		       (proxy [ActionListener] []
			 (actionPerformed [e#] ~@body))))
					    
(def swing-agent (agent nil))
		       
(defmacro swing [& body]
  `(send swing-agent 
	 (fn [_#]
	   (javax.swing.SwingUtilities/invokeLater (fn [] ~@body)))))

(defmacro swing-sync [& body]
  `(swing (dosync ~@body)))

(defmacro swing-io! [& body]
  `(if (not (javax.swing.SwingUtilities/isEventDispatchThread))
     (throw (RuntimeException. "Not in swing event dispatch thread!!!"))
     (io! 
      ~@body)))

(defn on-click [widget fun]
  (.addMouseListener widget
		     (proxy [MouseAdapter] []
		       (mouseClicked [e] (fun e)))))

(defn click-listener [fun]
  (proxy [MouseAdapter] [] (mouseClicked [e] (fun e))))

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

(defn create-panel [& widget-layouts]
  (let [panel (new JPanel (new MigLayout))]
    (doseq [[widget layout] widget-layouts]
      (.add panel widget layout))
    panel))

(defn create-panelS [{:keys [width height]} & widget-layouts]
  (let [p (apply create-panel widget-layouts)]
    (.setPreferredSize p (new Dimension width height))
    p))

(defn button [name fun]
  (let [b (new JButton name)]
    (.addActionListener b
     (proxy [ActionListener] []
       (actionPerformed [e] (fun e))))
    b))
