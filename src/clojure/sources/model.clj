(ns sources.model
  (:use gutil global)
  (:import (javax.swing JOptionPane)))

(defstruct File
  :lasfile
  :curve-list
  :view)


(defstruct SourceManager
  :sources
  :selected-source
  :source-tree
  :curve-panel
  :widget)

;; file-menu

(defn open-file [file]
  (try 
   (lasso/load-lasfile (.getPath file))
   (catch Exception e 
     (JOptionPane/showMessageDialog 
      (:frame @app) (str "There was an error reading file " (.getPath file))
      "Read Error" JOptionPane/ERROR_MESSAGE)
     (throw e))))

(defn open-files [files]
  (doall (map open-file files)))

(defn save-file [file]
  (let [lasfile (:lasfile @file)]
    (try 
     (lasso/save-lasfile lasfile)
     (catch Exception e
       (JOptionPane/showMessageDialog
	(:frame @app) (str "There was an error saving " (:name @lasfile))
	"Save Error" JOptionPane/ERROR_MESSAGE)
       (throw e)))))