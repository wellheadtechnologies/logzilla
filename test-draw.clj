(use 'curves 'lasso 'global 'util 'gutil)
(require 'sources.view)
(import '(javax.swing JFrame JPanel JScrollPane JList)
	'(java.awt Dimension)
	'(java.awt.event ComponentAdapter)
	'(net.miginfocom.swing MigLayout))

(def lasfile (load-lasfile "las_files/dollie.las"))

(def curve-list (sources.view/create-curve-list))

(let [frame (JFrame.)
      panel (JPanel. (MigLayout. "ins 0"))]
  (doto panel 
    (.add curve-list "push, grow"))
  (long-task
   (doseq [curve (:curves @lasfile)]
     (let [icon (curve-to-icon curve)]
       (swing
	(.addElement (.getModel curve-list) icon)
	(.repaint curve-list)))))
  (doto frame
    (.add panel)
    (.pack)
    (.setVisible true)))