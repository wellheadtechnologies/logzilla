(ns content.controller
  (:use content.view content.model)
  (:import (java.awt Dimension Color)
	   (javax.swing JPanel)))

(defn init-content-manager []
  (let [content-manager (ref nil)
	content-widget (create-content-widget)]
    (dosync 
     (ref-set content-manager
	      (struct-map ContentManager
		:widget content-widget)))
    content-manager))