(ns global
  (:import (java.util.concurrent Executors)))

(def task-executor (agent nil))

(def cached-executor (Executors/newCachedThreadPool))

(defmacro long-task [& body]
  `(send task-executor 
	 (fn [_#]
	   (.execute cached-executor (fn [] ~@body)))))
