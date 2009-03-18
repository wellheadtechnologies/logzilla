(ns gui.app
  (:use util gui.util gui.las gui.files)
  (:import (javax.swing JFrame JPanel JSlider
			JMenu JMenuItem JMenuBar
			UIManager JFileChooser
			JTabbedPane JDesktopPane
			JInternalFrame JScrollPane)
	   (java.awt Dimension)
	   (java.awt.event MouseMotionAdapter)
	   (net.miginfocom.swing MigLayout)))

(def main-frame (new JFrame "Curve Editor"))
(def main-panel (new JPanel (new MigLayout)))
(def menu-bar (new JMenuBar))
(def las-menu 
     (let [menu (new JMenu "Las")]
       (actions menu
	 ["Open" (fn [e] (open-files (user-selected-files "." main-frame)))]
	 ["Save All" (fn [e] nil)]
	 ["Quit" (fn [e] (System/exit 0))])
       menu))

(def main-width 500)
(def main-height 750)

(defn run-main []
  (UIManager/setLookAndFeel "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel")
  (doto main-panel
    (.setPreferredSize (new Dimension main-width main-height))
    (.add file-panel "pushy, growy, width 25%")
    (.add las-panel "pushy, growy, width 75%"))
  (.add menu-bar las-menu)
  
  (swing 
   (doto main-frame
     (.add main-panel)
     (.setJMenuBar menu-bar)
     (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
     (.pack)
     (.setResizable false)
     (.setVisible true))))