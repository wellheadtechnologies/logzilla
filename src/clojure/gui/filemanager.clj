(ns gui.filemanager
  (:use util gui.util gui.lfview))
(import '(org.apache.commons.io FileUtils)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel SwingUtilities
		      JFileChooser JMenu JPopupMenu)
	'(java.io File)
	'(core DefaultLasParser)
	'(net.miginfocom.swing MigLayout))

(def file-list (agent {}))
(def file-model (new DefaultListModel))
(def file-jlist 
     (let [list (new JList file-model)]
       (on-click list
	 (fn [e]
	   (when (= (.getClickCount e) 2)
	     (let [index (.locationToIndex file-jlist (.getPoint e))]
	       (send file-list 
		     #(open-lfview (get % (.getElementAt file-model index))))))))
       list))


(defn add-las-file [name las-file]
  (send file-list
	(fn [files]
	  (let [nfiles (assoc files name las-file)]
	    (swing 
	     (.addElement file-model name)
	     (.repaint file-jlist))
	    nfiles))))

(defmulti open-file class)

(defmethod open-file File [file]
  (let [lf (DefaultLasParser/parseLasFile file)]
    (add-las-file (.getName file) lf)))

(defmethod open-file String [path]
  (open-file (new File path)))
    
(defn open-files [files]
  (doseq [file files]
    (open-file file)))

(defn user-selected-files [cwd parent]
  (let [chooser (new JFileChooser cwd)]
    (.setMultiSelectionEnabled chooser true)
    (let [result (.showOpenDialog chooser parent)]
      (if (= JFileChooser/APPROVE_OPTION result)
	(.getSelectedFiles chooser)
	[]))))

(defn create-file-panel []
  (let [outer-panel (new JPanel (new MigLayout))
	inner-panel (new JPanel (new MigLayout))
	scroll-pane (new JScrollPane inner-panel)]
    (doto inner-panel
      (.add file-jlist "pushx, growx, pushy, growy"))
    (doto outer-panel 
      (.add scroll-pane "pushy, growy, width 25%"))
    outer-panel))

(def file-panel (create-file-panel))