(ns files.model
  (:use util global database)
  (:import (java.io File)))

(defn open-file [file]
  (let [proc (exec (str "lasso load " (.getPath file)))
	lf (database/load-lasfile (.getName file))]
    lf))

(defn open-files [files]
  (doall (map open-file files)))

(defn open-files-in-directory [path]
  (let [directory (new File path)
	files (.listFiles directory)]
    (open-files files)))

