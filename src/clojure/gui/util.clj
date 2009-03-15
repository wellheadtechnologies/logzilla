(ns gui.util
  (:use util))

(import '(java.awt.event ActionListener)
	'(javax.swing JFileChooser JMenu JPopupMenu SwingUtilities)
	'(java.awt.event MouseAdapter))

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
					    
		       
(defmacro swing [& body]
  `(javax.swing.SwingUtilities/invokeLater 
    (fn [] ~@body)))

(defn on-click [widget fun]
  (.addMouseListener widget
		     (proxy [MouseAdapter] []
		       (mouseClicked [e] (fun e)))))


