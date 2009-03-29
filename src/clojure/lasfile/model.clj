(ns lasfile.model 
  (:load "/lasso")
  (:use util))

(defn open-file [file]
  (lasso/load-lasfile (.getPath file)))

(defn open-files [files]
  (doall (map open-file files)))

