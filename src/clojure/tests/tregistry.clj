(ns tests.tregistry
  (:use registry global))

(deflogger tregistry)

(defn test-registry []
  (register :frames "frame 1")
  (register :frames "frame 2")
  (assert (some #(= "frame 1" %) (lookup :frames)))
  (assert (some #(= "frame 2" %) (lookup :frames)))
  (unregister :frames "frame 1")
  (assert (not (some #(= "frame 1" %) (lookup :frames))))
  (unregister :frames "frame 2")
  (assert (not (some #(= "frame 2" %) (lookup :frames))))
  (info "registry works"))

(defn run-tests []
  (test-registry))
