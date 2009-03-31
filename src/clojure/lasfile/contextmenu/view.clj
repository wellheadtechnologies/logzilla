(ns lasfile.contextmenu.view
  (:use gutil util global))

(defn create-context-menu [{:keys [curve-list x y] :as config}]
  (context-menu [curve-list x y]
    ["Edit" (:edit-action config)]
    ["Copy" (:copy-action config)]
    ["Paste" (:paste-action config)]
    ["Remove" (:remove-action config)]))

