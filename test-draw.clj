(use 'curves 'lasso)
(import '(javax.swing JFrame JPanel)
	'(net.miginfocom.swing MigLayout))

(def test1 (load-lasfile "las_files/test.las"))
(def curve1 (first (:curves @test1)))

(let [frame (JFrame.)
      panel (JPanel. (MigLayout. "wrap 1"))]
  (doseq [curve (:curves @test1)]
    (.add panel (curve-to-icon curve)))
  (doto frame
    (.add panel)
    (.pack)
    (.setVisible true)))