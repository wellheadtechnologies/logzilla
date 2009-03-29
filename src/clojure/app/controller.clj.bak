(ns app.controller
  (:use app.view app.model global util gutil files.controller))

(defn run-main []
  (binding [main-width 500 
	    main-height 700
	    main-frame (create-main-frame)
	    main-panel (create-main-panel)
	    menu-bar (create-menu-bar)
	    file-panel (init-file-panel)
	    file-menu (init-file-menu)
	    las-panel (init-las-panel)]
    (create-main-window)))