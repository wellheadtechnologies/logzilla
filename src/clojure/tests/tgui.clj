(ns tests.tgui
  (:use app.controller sources.controller util gutil global)
  (:require editor.controller))

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
  (let [test1f (open-lasfile "las_files/test.las")
	test1 (:lasfile @test1f)]
    (assert (probe 10000 
		   (let [curve-count (count (:curves @test1))
			 gui-count (count (get-source-curves))]
		     (= curve-count gui-count))))
    (println "correct number of curves"))
  (close-application))

(defn test-open-editor []
  (start-application)
  (let [dollief (open-lasfile "las_files/dollie.las")
	dollie (:lasfile @dollief)
	editor (editor.controller/open-curve-editor dollie (first (:curves @dollie)))]
    (assert (probe 10000 true)))
  (close-application))

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

;(defn test-storage []
;  (try 
;   (store :foo 1)
;   (store :foo 2)
;   (fail)
;   (catch java.lang.RuntimeException e 
;     (println "success : storage prevented adding of duplicate ids"))))

(defn run-tests []
  (test-add-lasfile))
