(ns app.controller
  (:require file.controller)
  (:use app.view app.model gutil global util)
  (:import (java.awt.event WindowAdapter)
	   (javax.swing UIManager)))

;(System/setProperty "apple.laf.useScreenMenuBar" "true")
;(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))

(def exit-on-close 
  (proxy [WindowAdapter] []
    (windowClosing [e] (System/exit 0))))

(defn resize [app]
  (let [app @app
	frame (get app :frame)
	height (get app :height)
	width (get app :width)]
    (swing 
     (.setSize frame width height)
     (.repaint frame))))

(defstruct App 
  :width
  :height
  :frame
  :panel
  :menu-bar
  :file-menu
  :window-menu
  :lasfile-pane
  :window-listeners)

(defn init-app []
  (let [width 500
	height 700
	file-manager (file.controller/init-file-manager)
	frame (create-main-frame)
	panel (create-main-panel)
	menu-bar (create-menu-bar)
	file-menu  (file.controller/init-file-menu file-manager)
	window-menu (create-window-menu (fn [e] nil))
	lasfile-pane (:pane @file-manager)
	window-listeners [exit-on-close]]
    (struct App width height frame panel menu-bar file-menu window-menu lasfile-pane window-listeners)))

(defn run-main []
  (let [my-app (init-app)]
    (create-main-window my-app)
    (dosync (ref-set app my-app))))