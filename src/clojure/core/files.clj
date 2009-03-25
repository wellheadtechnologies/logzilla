(ns core.files
  (:use util gui.global)
  (:import (org.jlas DefaultLasParser)
	   (java.io File)))

(def add-las-file)

(defmulti open-file class)

(defmethod open-file File [file]
  (let [lf (DefaultLasParser/parseLasFile file)]
    (add-las-file (.getName file) lf)
    lf))

(defmethod open-file String [path]
  (open-file (new File path)))
    
(defn open-files [files]
  (doall (map open-file files)))

(defn open-files-in-directory [path]
  (let [directory (new File path)
	files (.listFiles directory)]
    (open-files files)))

