(ns gui.util
  (:use util))

(import '(java.awt.event ActionListener)
	'(javax.swing JFileChooser JMenu JPopupMenu))

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

(defn user-selected-files [cwd parent]
  (let [chooser (new JFileChooser cwd)]
    (.setMultiSelectionEnabled chooser true)
    (if (= JFileChooser/APPROVE_OPTION
	   (.showOpenDialog chooser parent))
      (.getSelectedFiles chooser)
      [])))

(defmacro on-action [widget & body]
  `(.addActionListener ~widget 
		       (proxy [ActionListener] []
			 (actionPerformed [e#] ~@body))))
					    
		       