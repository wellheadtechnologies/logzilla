(ns global)

(def task-executor (agent nil))

(defmacro short-task [& body]
  `(send task-executor (fn [_#] ~@body)))

(defmacro long-task [& body]
  `(send-off task-executor (fn [_#] ~@body)))
