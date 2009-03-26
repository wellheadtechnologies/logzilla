(ns app.view
  (:use gutil)
  (:import (javax.swing JFrame JPanel JMenuBar JMenu)
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(def main-width)
(def main-height)
(def main-frame)
(def main-panel)
(def menu-bar)
(def file-panel)
(def las-panel)
(def file-menu)

(defn create-main-frame [] (new JFrame "Logzilla"))
(defn create-main-panel [] (new JPanel (new MigLayout)))
(defn create-main-window []
  (doto main-panel
    (.setPreferredSize (new Dimension main-width main-height))
    (.add file-panel "pushy, growy, width 25%")
    (.add las-panel "pushy, growy, width 75%"))

  (.add menu-bar file-menu)

  (swing 
   (doto main-frame
     (.add main-panel)
     (.setJMenuBar menu-bar)
     (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
     (.pack)
     (.setResizable false)
     (.setVisible true))))

