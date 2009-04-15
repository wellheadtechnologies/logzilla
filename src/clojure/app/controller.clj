(ns app.controller
  (:require file.controller inspector.controller)
  (:use app.view app.model gutil global util)
  (:import (java.awt.event WindowAdapter)
	   (java.awt Dimension)
	   (javax.swing UIManager)))

;(System/setProperty "apple.laf.useScreenMenuBar" "true")
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

(defstruct App 
  :width
  :height
  :frame
  :panel
  :menu-bar
  :file-menu
  :window-menu
  :file-widget
  :window-listeners)

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
	file-manager (file.controller/init-file-manager)
	frame (create-main-frame)
	panel (create-main-panel)
	menu-bar (create-menu-bar)
	file-menu  (file.controller/init-file-menu file-manager)
	window-menu (create-window-menu (fn [e] (inspector.controller/open-inspector)))
	file-widget (:pane @file-manager)
	window-listeners [exit-on-close]]
    (struct-map App
      :width width
      :height height
      :frame frame
      :panel panel
      :menu-bar menu-bar
      :file-menu file-menu
      :window-menu window-menu
      :file-widget file-widget
      :window-listeners window-listeners)))

(defn run-main []
  (let [my-app (init-app)]
    (create-main-window my-app)
    (dosync (ref-set app my-app))))