(ns app.view
  (:use gutil)
  (:import (javax.swing JFrame JPanel JMenuBar JMenu)
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn create-main-frame [] (JFrame. "Logzilla"))
(defn create-main-panel [] (JPanel. (MigLayout. "ins 0")))
(defn create-menu-bar [] (new JMenuBar))
(defn create-window-menu [open-inspector]
  (let [menu (new JMenu "Windows")]
    (actions menu
      ["Inspector" open-inspector])
    menu))

(defn create-main-window [{:keys [width height frame panel 
				  menu-bar file-menu file-widget
				  window-listeners window-menu]}]
  (doto panel
    (.setPreferredSize (new Dimension width height))
    (.add file-widget "push, grow"))
  (.add menu-bar file-menu)
  (.add menu-bar window-menu)
  (swing 
    (doseq [wl window-listeners]
      (.addWindowListener frame wl))
    (doto frame
      (.add panel)
      (.setJMenuBar menu-bar)
      (.pack)
      (.setResizable false)
      (.setVisible true))))