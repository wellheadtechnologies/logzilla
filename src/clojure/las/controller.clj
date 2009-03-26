(ns las.controller
  (:load "view")
  (:use las.model global util gutil global curves.model))

(def las-curves (agent {})) ;; lasfile -> [jlist curves]
(def file-views (agent {}))
(def current-file-view (agent (las.view/create-curve-panel)))

(defn add-curve [lasfile curve]
  (let [icon (curve-to-icon curve)]
    (send las-curves 
	  (fn [lcs]
	    (let [[jlist curves] (get lcs lasfile)]
	      (swing (.addElement (.getModel jlist) icon))
	      (assoc lcs lasfile [jlist (conj curves curve)])
	      )))))

(defn remove-curve [lasfile curve]
  (send las-curves
	(fn [lcs]
	  (let [[jlist curves] (get lcs lasfile)
		index (index-of curve curves)]
	    (swing (.removeElementAt (.getModel jlist) index))
	    (assoc lcs lasfile [jlist (remove #(= curve %) curves)])
	    ))))		   

(defn add-lasfile [lasfile]
  (let [curves (:curves lasfile)]
    (send las-curves
	  (fn [lcs]
	    (long-task
	     (doseq [curve curves]	       
	       (add-curve lasfile curve)))
	    (assoc lcs lasfile [jlist []])))))

(defn copy-curve [curve]
  (send copied-curves identity curve))

(defn- get-selected-curves [lasfile]
  (let [[jlist curves] (get @las-curves lasfile)
	selected (map #(.getText %) (.getSelectedValues jlist))]
    (filter (fn [curve]
	      (let [name (.getMnemonic curve)]
		(some #(= name %) selected)))
	    curves)))

(defn edit-action [lasfile selected-curves]
  (fn [e]
    (long-task (open-curve-editor lasfile selected-curves))))

(defn copy-action [selected-curves]
  (fn [e]
    (send copied-curves (fn [_] selected-curves))))

(defn paste-action [lasfile]
  (fn [e]
    (doseq [curve @copied-curves]
      (add-curve lasfile curve))))

(defn remove-action [lasfile selected-curves]
  (fn [e]
    (doseq [curve selected-curves]
      (remove-curve lasfile curve))))

(defn jlist-click-action [lasfile]
  (fn [e]
    (cond 
     (and (= MouseEvent/BUTTON1 (.getButton e))
	  (= 2 (.getClickCount e)))
     (swing (open-curve-editor (get-selected-curves lasfile)))
     
     (= MouseEvent/BUTTON3 (.getButton e))
     (swing (open-curves-context-menu lasfile e)))))

(defn- open-curves-context-menu [lasfile event]
  (let [[jlist curves] (get @las-curves lasfile)
	[c x y] [(.getComponent event) (.getX event) (.getY event)]
	scurves (get-selected-curves lasfile)]
    (create-context-menu c x y)))

(defn set-las-view [new-view]
  (send current-file-view
	(fn [old-view]
	  (swing
	   (doto las-panel
	     (.remove old-view)
	     (.add new-view)
	     (.revalidate))
	   (doto new-view
	     (.repaint)
	     (.revalidate)))
	  new-view)))

(defn open-las-view [lasfile]
  (send file-views
	(fn [views]
	  (let [existing-view (get views lasfile)]
	    (if existing-view
	      (do 
		(set-las-view existing-view)
		views)
	      (let [new-view (create-las-view lasfile)]
		(do
		  (set-las-view new-view)
		  (assoc views lasfile new-view))))))))

