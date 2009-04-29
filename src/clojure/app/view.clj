(ns app.view
  (:use gutil)
  (:import (javax.swing JFrame JPanel JMenuBar JMenu)
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn create-window-menu [open-inspector]
  (let [menu (JMenu. "Windows")]
    (actions menu
      ["Inspector" open-inspector])
    menu))

(defn create-application [{:keys [file-menu window-menu
				  sources-widget 
				  content-widget]}]
  (let [sources-frame (JFrame. "Sources")
	sources-panel (JPanel. (MigLayout. "ins 0"))
	sources-menu-bar (JMenuBar.)
	content-frame (JFrame. "Content")
	content-panel (JPanel. (MigLayout. "ins 0"))
	content-menu-bar (JMenuBar.)]

    (doto sources-panel
      (.setPreferredSize (Dimension. 500 700))
      (.add sources-widget "push, grow"))
    (doto sources-menu-bar
      (.add file-menu)
      (.add window-menu))

    (doto content-panel
      (.setPreferredSize (Dimension. 800 700))
      (.add content-widget "push, grow"))

    (swing 
     (doto sources-frame
       (.add sources-panel)
       (.setJMenuBar sources-menu-bar)
       (.pack)
       (.setResizable true)
       (.setVisible true))
     
     (doto content-frame
       (.add content-panel)
       (.setJMenuBar content-menu-bar)
       (.pack)
       (.setResizable true)
       (.setVisible true)))
    {:sources-frame sources-frame
     :content-frame content-frame}))