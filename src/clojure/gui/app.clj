(ns gui.app
  (:use util gui.util gui.lfview
	gui.filemanager))

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

(UIManager/setLookAndFeel "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel")

(defn setup-main-frame []
  (let [las-menu (create-las-menu)]      
    (doto main-panel
      (.setPreferredSize (new Dimension 500 750))
      (.add file-panel "pushy, growy, width 25%")
      (.add curve-panel "pushy, growy, width 75%"))
    (.add menu-bar las-menu)

    (swing 
     (doto main-frame
       (.add main-panel)
       (.setJMenuBar menu-bar)
       (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
       (.pack)
       (.setResizable false)
       (.setVisible true)))))