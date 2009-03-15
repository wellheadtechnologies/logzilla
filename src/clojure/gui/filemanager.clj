(ns gui.filemanager
  (:use util gui.util gui.lfview gui.widgets))
(import '(org.apache.commons.io FileUtils)
	'(javax.swing JList JFrame DefaultListModel ImageIcon JLabel
		      JScrollPane JButton JWindow JPanel SwingUtilities
		      JFileChooser JMenu JPopupMenu)
	'(java.io File)
	'(core DefaultLasParser)
	'(gui LasFileList)
	'(net.miginfocom.swing MigLayout))

(def file-list 
     (let [list (new LasFileList)]
       (on-click list
	 (fn [e]
	   (open-curve-view (.getSelectedLasFile list))))
       list))

(defn add-las-file [name las-file]
  (.addLasFile file-list las-file))

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
  (let [outer-panel (create-titled-panel "Las Files")
	inner-panel (new JPanel (new MigLayout))
	scroll-pane (new JScrollPane inner-panel)]
    (doto inner-panel
      (.add file-list "pushy, growy, pushx, growx"))
    (doto outer-panel 
      (.add scroll-pane "pushy, growy, pushx, growx"))
    outer-panel))

(def file-panel (create-file-panel))

