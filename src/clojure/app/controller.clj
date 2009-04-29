(ns app.controller
  (:require sources.controller inspector.controller
	    content.controller)
  (:use app.view app.model gutil global util)
  (:import (java.awt.event WindowAdapter)
	   (java.awt Dimension Color)
	   (javax.swing UIManager)))

(System/setProperty "apple.laf.useScreenMenuBar" "true")
(UIManager/put "Table.alternateRowColor" Color/BLUE)
;(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))


(defn init-app []
  (let [source-manager (sources.controller/init-source-manager)
	content-manager (content.controller/init-content-manager)
	file-menu  (sources.controller/init-file-menu source-manager)
	window-menu (create-window-menu (fn [e] (inspector.controller/open-inspector)))
	sources-widget (:widget @source-manager)
	content-widget (:widget @content-manager)
	{:keys [sources-frame content-frame]} (create-application 
					       {:file-menu file-menu
						:window-menu window-menu
						:sources-widget sources-widget
						:content-widget content-widget})]
    (struct-map App
      :sources-frame sources-frame
      :content-frame content-frame)))

(defn start-application []
  (let [my-app (init-app)]
    (dosync (ref-set app my-app))))