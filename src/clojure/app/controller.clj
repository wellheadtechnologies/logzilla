(ns app.controller
  (:require lasfile.controller global [app.state :as state])
  (:use app.view app.model)
  (:import (java.awt.event WindowAdapter)))

(defstruct AppConfig 
  :width 
  :height 
  :frame 
  :panel
  :menu-bar
  :lasfile-pane)

(def exit-on-close 
  (proxy [WindowAdapter] []
    (windowClosed [e] (System/exit 0))))

(defn get-default-config []
  (struct-map AppConfig
    :width 500 
    :height 700
    :frame (create-main-frame)
    :panel (create-main-panel)
    :menu-bar (create-menu-bar)
    :file-menu (lasfile.controller/init-file-menu)
    :lasfile-pane (lasfile.controller/init-lasfile-pane)
    :window-listeners [exit-on-close]))

(defn run-main []
  (let [config (get-default-config)]
    (send state/app-config (fn [_] config))
    (create-main-window config)))

(defn open-main []
  (let [config (get-default-config)
	config (assoc config :window-listeners [])]
    (send state/app-config (fn [_] config))
    (create-main-window config)))