(ns lasfile.view
  (:use gutil)
  (:import (javax.swing JMenu JFileChooser JPanel 
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(defn create-curve-panel []
  (let [panel (new JPanel (new MigLayout))]
    (doto panel
      (.setBorder (BorderFactory/createEtchedBorder)))))

(defn create-curve-list []
  (let [jlist (new JList (new DefaultListModel))]
    (doto jlist
      (.setFixedCellHeight 80)
      (.setCellRenderer (new IconListCellRenderer))
      (.setBackground (.getBackground (new JPanel)))
      (.setOpaque false))))

(defn create-lasfile-view [{:keys [las-file curve-list]}]
  (let [inner-panel (create-inner-panel)
	pane (new JScrollPane inner-panel)
	outer-panel (create-curve-panel)]
    (doto inner-panel
      (.add curve-list "pushx, growx, pushy, growy, wrap"))
    (doto outer-panel
      (.add pane "pushx, pushy, growx, growy, wrap"))))

(defn create-lasfile-pane []
  (new JTabbedPane))