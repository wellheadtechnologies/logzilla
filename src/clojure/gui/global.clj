(ns gui.global)

(import '(java.util.concurrent Executors)
	'(java.lang.management ManagementFactory OperatingSystemMXBean))

(def copied-curves (agent []))

(def osbean (ManagementFactory/getOperatingSystemMXBean))

(def fixed-executor-service (Executors/newFixedThreadPool (.getAvailableProcessors osbean)))

(def cached-executor-service (Executors/newCachedThreadPool))

(defmacro short-task [& body]
  `(.execute fixed-executor-service (fn [] ~@body)))

(defmacro long-task [& body]
  `(.execute cached-executor-service (fn [] ~@body)))
