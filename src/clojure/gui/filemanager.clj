(ns gui.filemanager
  (:use util parser writer gui.util))
(import '(org.apache.commons.io FileUtils)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel SwingUtilities
		      JFileChooser JMenu JPopupMenu)
	'(java.io File)
	'(net.miginfocom.swing MigLayout))

(def debug true)
(defn dprintln [& args]
  (when debug (apply println args)))

(def file-list (ref {}))
(def file-cmodel (new DefaultListModel))
(def file-jlist (new JList file-cmodel))

(defn add-las-file [name las-file]
  (dosync (alter file-list assoc name las-file))
  (swing 
   (.addElement file-cmodel name)
   (.repaint file-jlist)))

(defmulti open-file class)

(defmethod open-file File [file]
  (let [lf (parse-las-file (FileUtils/readFileToString file))]
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
