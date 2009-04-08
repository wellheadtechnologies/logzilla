(ns global
  (:import (java.util.concurrent Executors)))

(def task-executor (agent nil))
(def short-task-executor (agent nil))
(def print-executor (agent nil))

(def cached-executor (Executors/newCachedThreadPool))

(def fixed-executor (Executors/newFixedThreadPool 2))

(defmacro long-task [& body]
  `(send task-executor 
	 (fn [_#]
	   (.execute cached-executor (fn [] ~@body)))))

(defmacro short-task [& body]
  `(send short-task-executor 
	 (fn [_#]
	   (.execute fixed-executor (fn [] ~@body)))))

(defmacro print-task [& body]
  `(send print-executor 
	 (fn [_#]
	   (println ~@body))))

(def app (ref nil))

(def copied-curves (ref []))

(def file-methods (ref {}))

(defmacro fm-invoke [name & args]
  `(let [method# (get @file-methods ~name)]
     (method# ~@args)))

(def switch-inspector-tab-method (ref nil))

(defn switch-inspector-tab [& args]
  (apply @switch-inspector-tab-method args))