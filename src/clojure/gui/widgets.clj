(ns gui.widgets)
(import '(javax.swing JPanel JLabel)
	'(net.miginfocom.swing MigLayout))

(defn create-titled-panel [title]
  (let [title-panel (new JPanel)
	outer-panel (new JPanel (new MigLayout))]
    (doto title-panel
      (.add (new JLabel title)))
    (doto outer-panel
      (.add title-panel "growx, wrap"))
    outer-panel))
    