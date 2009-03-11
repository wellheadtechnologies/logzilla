(ns gui.util
  (:use util))

(import '(java.awt.event ActionListener)
	'(javax.swing JFileChooser))

(defn actions [menu & name-actions]
  (doseq [[name action] name-actions]
    (let [item (.add menu name)]
      (.addActionListener item (proxy [ActionListener] []
				 (actionPerformed [e] (action e)))))))
			  

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
					    
		       