(ns tests.all
  (:require tests.tgui tests.tregistry))

(defn run-tests []
  (tests.tgui/run-tests)
  (tests.tregistry/run-tests)
  (System/exit 0))
