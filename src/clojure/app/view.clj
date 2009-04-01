(ns app.view
  (:use gutil)
  (:import (javax.swing JFrame JPanel JMenuBar JMenu)
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn create-main-frame [] (new JFrame "Logzilla"))
(defn create-main-panel [] (new JPanel (new MigLayout)))
(defn create-menu-bar [] (new JMenuBar))
(defn create-main-window [{:keys [width height frame panel menu-bar file-menu lasfile-pane] :as config}]
  (doto panel
    (.setPreferredSize (new Dimension width height))
    (.add lasfile-pane "pushy, growy, pushx, growx"))
  (.add menu-bar file-menu)
  (swing 
    (doseq [wl (:window-listeners config)]
      (.addWindowListener frame wl))
    (doto frame
      (.add panel)
      (.setJMenuBar menu-bar)
      (.pack)
      (.setResizable false)
      (.setVisible true))))