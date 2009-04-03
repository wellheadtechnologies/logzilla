(ns tests.tgui
  (:require app.controller app.model
	    lasfile.controller lasfile.model
	    editor.controller editor.model
	    lasso)
  (:use util gutil global storage))

(defn fail [] (assert false))

(defn wait-for [[millis interval msg] fun]
  (let [start (System/currentTimeMillis)
	probe (ref false)
	result (loop []
		 (fun probe)
		 (cond 
		   @probe true
		   (> (System/currentTimeMillis) (+ start millis)) false
		   :else 
		   (do
		     (Thread/sleep interval)
		     (recur))))]
    (if (= true result)
      (println "success : " msg)
      (do 
	(println "failure : " msg)
	(fail)))))

(defmacro swing-probe [& body]
  `(fn [probe#]
     (swing-sync
      (let [result# (do ~@body)]
	(when result#
	  (ref-set probe# true))))))

(defn test-add-lasfile []
  (app.controller/async-open-main)
  (let [dollie (lasso/load-lasfile "las_files/dollie.las")]
    (invoke :add-lasfile dollie)
    (wait-for [10000 100 "added all curves"]
      (swing-probe 
       (= (count (:curves dollie)) 
	  (.. (lasfile.model/get-selected-curve-list) (getModel) (getSize))))))
  (app.controller/close-main))

(defn test-open-editor []
  (app.controller/async-open-main)
  (let [dollie-id (lasso/load-lasfile "las_files/dollie.las")
	dollie (lookup dollie-id)
	frame (editor.controller/open-curve-editor dollie-id (take 2 (:curves dollie)))]
    (wait-for [10000 100 "curve editor contains frame and curves"]
      (swing-probe 
       (and (contains? (lookup [frame :charts]) (first (:curves dollie)))
	    (contains? (lookup [frame :charts]) (nth (:curves dollie) 1))
	    (not (contains? (lookup [frame :charts]) (nth (:curves dollie) 2))))))))

(defn test-sync-curve-with-table []
  (app.controller/async-open-main)
  (let [test1-id (lasso/load-lasfile "las_files/test.las")
	test1 (storage/lookup test1-id)
	frame (editor.controller/open-curve-editor test1-id (take 2 (:curves test1)))
	index 0]
    (swing
      (let [table (lookup-in [frame :widgets] :table)]
	(.setValueAt (.getModel table) 10 (editor.model/index-to-row 0 table) 1)))
    (wait-for [10000 100 "dirty-curve(0) == 10 after syncing with table"]
      (swing-probe
       (let [curve-id (first (:curves test1))
	     dirty-curve (lookup-in [frame :charts] curve-id :dirty-curve)]
	 (= (nth (:data dirty-curve) 0) 10))))))

(defn test-storage []
  (try 
   (store :foo 1)
   (store :foo 2)
   (fail)
   (catch java.lang.RuntimeException e 
     (println "success : storage prevented adding of duplicate ids"))))

(defn run-tests []
  (test-add-lasfile)
  (test-open-editor)
  (test-sync-curve-with-table)
  (test-storage)
  (System/exit 0))
