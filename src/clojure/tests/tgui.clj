(ns tests.tgui
  (:use app.controller sources.controller util gutil global)
  (:require editor.controller chart.controller
	    [editor.table.controller :as table-controller]))

(deflogger tgui)

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
  (let [test1 (open-source :content "las_files/test.las")]
    (assert (probe 10000 
		   (let [curve-count (count (:curves @test1))
			 gui-count (count (get-source-curves))]
		     (= curve-count gui-count)))))
  (info "correct number of curves")
  (close-application))

(defn test-open-editor []
  (start-application)
  (let [dollie (open-source :content "las_files/dollie.las")
	editor (editor.controller/open-curve-editor dollie (first (:curves @dollie)))]
    (assert (probe 10000 true)))
  (info "open editor works")
  (close-application))

(defn test-sync-chart-with-table []
  (start-application)
  (let [robert (open-source :content "las_files/robert.las")
	curve (first (:curves @robert))
	editor (editor.controller/open-curve-editor robert curve)
	index 0
	{:keys [table chart]} @editor]
    (swing-agent (table-controller/set-value table {:row 0 :col 1} 10))
    (assert (probe 10000 (= 10 (chart.controller/get-value chart {:curve 0 :index 0})))))
  (info "sync-chart-with-table works")
  (close-application))

(defn test-sync-table-with-chart []
  (start-application)
  (let [test1 (open-source :content "las_files/test.las")
	curve (first (:curves @test1))
	editor (editor.controller/open-curve-editor test1 curve)
	index 12]
    (swing-agent 
     (let [chart (:chart @editor)]
       (chart.controller/set-value chart {:curve 0 :index 12} 3)))
    (assert 
     (probe 10000
	    (let [table (:table @editor)]
	      (= (table-controller/get-value table {:row index :col 1}) 3))))
    (info "sync-table-with-chart works"))
  (close-application))

(defn run-tests []
  (test-add-lasfile)
  (test-open-editor)
  (test-sync-chart-with-table)
  (test-sync-table-with-chart))
