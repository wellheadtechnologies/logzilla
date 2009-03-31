(ns lasfile.filemenu.model
  (:require lasso))

(defn open-file [file]
  (lasso/load-lasfile (.getPath file)))

(defn open-files [files]
  (doall (map open-file files)))

