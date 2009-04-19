(ns app.controller
  (:require sources.controller inspector.controller)
  (:use app.view app.model gutil global util)
  (:import (java.awt.event WindowAdapter)
	   (java.awt Dimension Color)
	   (javax.swing UIManager)))

(System/setProperty "apple.laf.useScreenMenuBar" "true")
(UIManager/put "Table.alternateRowColor" Color/BLUE)
;(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))

(def exit-on-close 
  (proxy [WindowAdapter] []
    (windowClosing [e] (System/exit 0))))

(defn resize [app]
  (let [{:keys [frame height width]} @app]
    (swing 
     (doto frame
       (.setSize width height)
       (.repaint)))))

(def size-watcher (agent []))

(defn resize [[old-width old-height] app]
  (dosync 
   (let [{:keys [width height frame]} @app]
     (when (or (not= width old-width)
	       (not= height old-height))
       (swing 
	(.setSize frame (Dimension. width height))
	(.repaint frame)))
     [width height])))

(add-watcher app :send size-watcher resize)

(defn init-app []
  (let [width 500
	height 700
	source-manager (sources.controller/init-source-manager)
	frame (create-main-frame)
	panel (create-main-panel)
	menu-bar (create-menu-bar)
	file-menu  (sources.controller/init-file-menu source-manager)
	window-menu (create-window-menu (fn [e] (inspector.controller/open-inspector)))
	sources-widget (:widget @source-manager)
	window-listeners [exit-on-close]]
    (struct-map App
      :width width
      :height height
      :frame frame
      :panel panel
      :menu-bar menu-bar
      :file-menu file-menu
      :window-menu window-menu
      :sources-widget sources-widget
      :window-listeners window-listeners)))

(defn run-main []
  (let [my-app (init-app)]
    (create-main-window my-app)
    (dosync (ref-set app my-app))))