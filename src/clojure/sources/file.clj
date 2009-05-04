(ns sources.controller)

(declare create-curve-list-view File custom-tree-payload)

(defn- create-file-view [curve-list-view header-edit-button]
  (let [panel (JPanel. (MigLayout. "ins 0, nogrid"))]
    (doto panel
      (.add curve-list-view "push, grow, spanx 2, wrap")
      (.add header-edit-button "alignx 50%, wrap"))))

(defn- init-file [source-manager lasfile]
  (let [curve-list (init-curve-list source-manager (:curves @lasfile))
	curve-list-view (create-curve-list-view curve-list)
	edit-headers-button (header-dialog/init-edit-button lasfile)
	panel (create-file-view curve-list-view edit-headers-button)
	file (struct-map File
	       :lasfile lasfile
	       :curve-list curve-list
	       :view panel)]
    (ref file)))