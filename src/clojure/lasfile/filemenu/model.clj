(ns lasfile.filemenu.model
  (:require lasso))

(defstruct FileMenuConfig :open-action :save-all-action :quit-action)

(defn open-file [file]
  (lasso/load-lasfile (.getPath file)))

(defn open-files [files]
  (doall (map open-file files)))

