(ns gui.global)

(import '(java.util.concurrent Executors))

(def copied-curves (agent []))

(def executor-service (Executors/newCachedThreadPool))

(defmacro execute-later [& body]
  `(.execute executor-service (fn [] ~@body)))