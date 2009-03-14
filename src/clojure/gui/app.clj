(ns gui.app)
(use 'util)
(use 'gui.util)
(use 'gui.lfview)
(use 'parser)
(use 'writer)
(use 'gui.filemanager)

(import '(javax.swing JFrame JPanel JSlider
		      JMenu JMenuItem JMenuBar
		      UIManager JFileChooser
		      JTabbedPane JDesktopPane
		      JInternalFrame JScrollPane)
	'(java.awt Dimension)
	'(java.awt.event MouseMotionAdapter)
	'(net.miginfocom.swing MigLayout))

(def main-panel-size (new Dimension 800 800))
(def main-frame (new JFrame "Curve Editor"))
(def main-panel (new JPanel (new MigLayout)))
(def menu-bar (new JMenuBar))

(defn create-las-menu []
  (let [menu (new JMenu "Las")]
    (actions menu
      ["Open" (fn [e] (open-files (user-selected-files "." main-frame)))]
      ["Save All" (fn [e] nil)]
      ["Quit" (fn [e] (System/exit 0))])
    menu))

;(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
(UIManager/setLookAndFeel "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel")

(defn setup-main-frame []
  (let [las-menu (create-las-menu)]      
    (doto main-panel
      (.add file-panel "pushx, pushy, growx, growy")
      (.add lfview-panel)
      (.setPreferredSize main-panel-size))
    
    (.add menu-bar las-menu)

    (swing 
     (doto main-frame
       (.add main-panel)
       (.setJMenuBar menu-bar)
       (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
       (.pack)
       (.setVisible true)))))