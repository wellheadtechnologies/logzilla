(ns gui.app)

(import '(javax.swing JFrame JPanel JSlider)
	'(java.awt Dimension)
	'(net.miginfocom.swing MigLayout))

(def main-panel-size (new Dimension 400 700))

(let [frame (new JFrame "Curve Editor")
      main-panel (new JPanel (new MigLayout))
      depth-slider (new JSlider)]

  (doto depth-slider
    (.setOrientation (. JSlider VERTICAL)))

  (doto main-panel
    (.setPreferredSize main-panel-size)
    (.add depth-slider "pushy, growy"))

  (doto frame
    (.add main-panel)
    (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
    (.pack)
    (.setVisible true)))

