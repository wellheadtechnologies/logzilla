(ns app.controller
  (:require sources.controller inspector.controller
	    format.controller console.controller
	    registry)
  (:use gutil global util)
  (:import (javax.swing JFrame JPanel JMenuBar JMenu)
	   (java.awt.event WindowAdapter)
	   (net.miginfocom.swing MigLayout)
	   (java.awt Dimension Color)
	   (javax.swing UIManager)))

(System/setProperty "apple.laf.useScreenMenuBar" "true")
(UIManager/put "Table.alternateRowColor" Color/BLUE)
;(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))

(defstruct App 
  :sources-frame)

(defn create-window-menu []
  (let [menu (JMenu. "Windows")]
    (actions menu
      ["Inspector" (fn [e] (inspector.controller/open-inspector))]
      ["Format" (fn [e] (format.controller/open-formatter))]
      ["Console" (fn [e] (console.controller/open-console))])
    menu))

(defn create-application [{:keys [file-menu window-menu sources-widget]}]
  (let [sources-frame (registry/acquire-registered-frame)
	sources-panel (JPanel. (MigLayout. "ins 0"))
	sources-menu-bar (JMenuBar.)]

    (doto sources-frame 
      (.setTitle "Sources"))

    (doto sources-panel
      (.setPreferredSize (Dimension. 500 700))
      (.add sources-widget "push, grow"))
    (doto sources-menu-bar
      (.add file-menu)
      (.add window-menu))

    (swing-agent
     (doto sources-frame
       (.add sources-panel)
       (.setJMenuBar sources-menu-bar)
       (.pack)
       (.setResizable true)
       (.setVisible true)))
    sources-frame))

(defn init-app []
  (let [source-manager (sources.controller/init-source-manager)
	file-menu  (sources.controller/init-file-menu source-manager)
	window-menu (create-window-menu)
	sources-widget (:widget @source-manager)
	sources-frame (create-application 
		       {:file-menu file-menu
			:window-menu window-menu
			:sources-widget sources-widget})]
    (struct-map App
      :sources-frame sources-frame
      :source-manager source-manager)))

(defn start-application []
  (let [my-app (init-app)]
    (dosync (ref-set app my-app))))

(defn close-application [])