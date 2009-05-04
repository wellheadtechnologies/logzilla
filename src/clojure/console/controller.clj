(ns console.controller
  (:import (javax.swing JFrame JPanel JScrollPane JTextArea)
	   (net.miginfocom.swing MigLayout)
	   (java.awt Dimension)
	   (gui JConsole)))

(defstruct Console
  :widget)

(defn init-console []
  (let [frame (JFrame.)
	console (JConsole.)]
    (doto frame
      (.add console)
      (.setSize (Dimension. 200 300))
      (.setVisible true))
    (struct-map Console
      :widget frame)))

(defn open-console [] (init-console))