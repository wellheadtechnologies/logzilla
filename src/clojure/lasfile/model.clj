(ns lasfile.model 
  (:load "/persistence")
  (:use util))

(defn open-file [file]
  (persistence/load-lasfile (.getPath file)))

(defn open-files [files]
  (doall (map open-file files)))

