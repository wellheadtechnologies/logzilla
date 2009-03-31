(ns app.controller
  (:require lasfile.controller global)
  (:use app.view app.model))

(defstruct AppConfig 
  :width 
  :height 
  :frame 
  :panel
  :menu-bar
  :lasfile-pane)

(defn get-default-config []
  (struct-map AppConfig
    :width 500 
    :height 700
    :frame (create-main-frame)
    :panel (create-main-panel)
    :menu-bar (create-menu-bar)
    :file-menu (lasfile.controller/init-file-menu)
    :lasfile-pane (lasfile.controller/init-lasfile-pane)))

(defn run-main []
  (let [config (get-default-config)]
    (send global/app-config (fn [_] config))
    (create-main-window config)))