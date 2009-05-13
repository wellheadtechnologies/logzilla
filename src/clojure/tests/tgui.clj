(ns tests.tgui
  (:use app.controller sources.controller util gutil global))

(defn fail [] (assert false))

(defmacro probe [timeout & body]
  `(let [start# (current-time)
	 fun# (fn [] (swing-getter ~@body))]
     (loop []
       (cond 
	(fun#) 
	true

	(> (current-time) (+ start# ~timeout)) 
	false

	:else
	(do
	  (Thread/sleep 100)
	  (recur))))))


(defn test-add-lasfile []
  (start-application)
  (let [dollief (open-lasfile "las_files/dollie.las")
	dollie (:lasfile @dollief)]
    (assert (probe 10000 
		   (let [curve-count (count (:curves @dollie))
			 gui-count (count (get-source-curves))]
		     (println "(curve-count " curve-count ") (gui-count " gui-count ")")
		     (= curve-count gui-count))))
    (println "correct number of curves")))

;(defn test-open-editor []
;  (app.controller/open-main)
;  (let [dollie-id (lasso/load-lasfile "las_files/dollie.las")
;	dollie (lookup dollie-id)
;	frame (editor.controller/open-curve-editor dollie-id (take 2 (:curves dollie)))]
;    (wait-for [10000 100 "curve editor contains frame and curves"]
;      (swing-probe 
;       (and (contains? (lookup [frame :charts]) (first (:curves dollie)))
;	    (contains? (lookup [frame :charts]) (nth (:curves dollie) 1))
;	    (not (contains? (lookup [frame :charts]) (nth (:curves dollie) 2))))))
;    (app.controller/close-main)))
;
;(defn test-sync-curve-with-table []
;  (app.controller/open-main)
;  (let [test1-id (lasso/load-lasfile "las_files/test.las")
;	test1 (storage/lookup test1-id)
;	frame (editor.controller/open-curve-editor test1-id (take 2 (:curves test1)))
;	index 0]
;    (swing
;      (let [table (lookup-in [frame :widgets] :table)]
;	(.setValueAt (.getModel table) 10 (editor.model/index-to-row 0 table) 1)))
;    (wait-for [10000 100 "dirty-curve(0) == 10 after syncing with table"]
;      (swing-probe
;       (let [curve-id (first (:curves test1))
;	     dirty-curve (lookup-in [frame :charts] curve-id :dirty-curve)]
;	 (= (nth (:data dirty-curve) 0) 10))))
;    (app.controller/close-main)))
;
;(defn test-storage []
;  (try 
;   (store :foo 1)
;   (store :foo 2)
;   (fail)
;   (catch java.lang.RuntimeException e 
;     (println "success : storage prevented adding of duplicate ids"))))

(defn run-tests []
  (test-add-lasfile)
  (System/exit 0))
