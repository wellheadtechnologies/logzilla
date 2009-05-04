(import '(javax.swing JFrame)
	'(gui JConsole))

(def frame (JFrame.))
(def console (JConsole.))

(doto frame 
  (.add console)
  (.pack)
  (.setVisible true))