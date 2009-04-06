(ns app.controller
  (:require lasfile.controller inspector.controller)
  (:use app.view app.model gutil global util storage)
  (:import (java.awt.event WindowAdapter)
	   (javax.swing UIManager)))

;(System/setProperty "apple.laf.useScreenMenuBar" "true")
;(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))

(def exit-on-close 
  (proxy [WindowAdapter] []
    (windowClosing [e] (System/exit 0))))

(defn change-width [width]
  (dosync 
   (let [frame (lookup-in :app :frame)
	 height (lookup-in :app :height)]
     (swing 
      (.setSize frame width height)
      (.repaint frame)))))

(defn change-height [height]
  (dosync 
   (let [frame (lookup-in :app :frame)
	 width (lookup-in :app :width)]
     (swing 
       (.setSize frame width height)
       (.repaint frame)))))

(defproperties app-props
  [:width 500 
   :on-change change-width]
  [:height 700
   :on-change change-height]
  [:frame nil
   :init create-main-frame]
  [:panel nil
   :init create-main-panel]
  [:menu-bar nil
   :init create-menu-bar]
  [:file-menu nil
   :init lasfile.controller/init-file-menu]
  [:window-menu nil
   :init (partial create-window-menu
		  {:open-inspector
		   (fn [e] (inspector.controller/open-inspector))})]
  [:lasfile-pane nil
   :init lasfile.controller/init-lasfile-pane]
  [:window-listeners [exit-on-close]])

(defn run-main []
  (let [iprops (init-properties app-props)
	props (store-properties :app iprops)]
    (create-main-window props)))

(defn open-main []
  (let [iprops (init-properties (assoc app-props :window-listeners []))
	props (store-properties :app iprops)]
    (create-main-window props)))

(defn close-main []
  (dosync 
   (let [frame (lookup-in :app :frame)]
     (swing (doto frame (.hide) (.dispose)))
     (unstore-properties :app))))
