(ns file.contextmenu.view
  (:use gutil util global))

(defn create-context-menu [curve-list x y cm-actions]
  (context-menu [curve-list x y]
    ["Edit" (:edit cm-actions)]
    ["Copy" (:copy cm-actions)]
    ["Paste" (:paste cm-actions)]
    ["Remove" (:remove cm-actions)]))

