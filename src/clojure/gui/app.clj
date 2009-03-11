(ns gui.app)
(use 'util)
(use 'gui.util)
(use 'gui.lfview)
(use 'parser)

(import '(javax.swing JFrame JPanel JSlider
		      JMenu JMenuItem JMenuBar
		      UIManager JFileChooser
		      JTabbedPane)
	'(java.awt Dimension)
	'(java.awt.event MouseMotionAdapter)
	'(net.miginfocom.swing MigLayout))

(def main-panel-size (new Dimension 400 700))
(def main-frame (new JFrame))
(def main-panel (new JPanel (new MigLayout)))
(def tab-pane (new JTabbedPane))

(defn open-files [files]
  (let [strings (map #(slurp (.getPath %)) files)
	lfs (map #(parse-las-file %) strings)]
    (doseq [[file lf] (tuplize files lfs)]
      (.addTab tab-pane (.getName file) (las-file-view lf)))
    (.revalidate main-panel)))

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
      menu-bar (new JMenuBar)]
  
  (doto main-panel
    (.setPreferredSize main-panel-size)
    (.add tab-pane "pushy, pushx, growy, growx"))

  (.add menu-bar file-menu)

  (doto main-frame
    (.add main-panel)
    (.setJMenuBar menu-bar)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.setVisible true)))

