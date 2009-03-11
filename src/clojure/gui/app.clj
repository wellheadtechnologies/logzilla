(ns gui.app)
(use 'gui.util)
(use 'parser)

(import '(javax.swing JFrame JPanel JSlider
		      JMenu JMenuItem JMenuBar
		      UIManager JFileChooser)
	'(java.awt Dimension)
	'(net.miginfocom.swing MigLayout))

(def main-panel-size (new Dimension 400 700))

(def main-frame (new JFrame))

(defn open-files [files]
  (doseq [path (map #(.getPath %) files)]
    (println path))
  (let [strings (map #(slurp (.getPath %)) files)
	lfs (map #(parse-las-file %) strings)]
    (doseq [lf lfs]
      (println (:name lf)))))

(defn create-file-menu []
  (let [menu (new JMenu "File")]
    (actions menu
      ["Open" (fn [e] (open-files (user-selected-files "." main-frame)))]
      ["Save" (fn [e] nil)]
      ["Save As" (fn [e] nil)]
      ["Quit" (fn [e] nil)])
    menu))
  
;(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))

(let [file-menu (create-file-menu)
      main-panel (new JPanel (new MigLayout))
      menu-bar (new JMenuBar)
      depth-slider (new JSlider)]

  (doto depth-slider
    (.setOrientation JSlider/VERTICAL))

  (doto main-panel
    (.setPreferredSize main-panel-size)
    (.add depth-slider "pushy, growy"))

  (.add menu-bar file-menu)

  (doto main-frame
    (.add main-panel)
    (.setJMenuBar menu-bar)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.setVisible true)))

