(ns file.headerdialog.controller
  (:use util gutil global)
  (:import (javax.swing JTable JScrollPane JButton JPanel JDialog JTabbedPane)
	   (javax.swing.table DefaultTableModel)
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn init-header-tab [header]
  (swing-io!
   (let [model (DefaultTableModel.)
	 table (JTable. model)
	 pane (JScrollPane. table)
	 descriptors (:descriptors @header)]
     (doto model
       (.addColumn "mnemonic" (into-array Object (map :mnemonic descriptors)))
       (.addColumn "unit" (into-array Object (map :unit descriptors)))
       (.addColumn "data" (into-array Object (map :data descriptors)))
       (.addColumn "description" (into-array Object (map :description descriptors))))
     pane)))

(defn- find-header [lasfile type]
  (find-first #(= type (:type (deref %))) (:headers @lasfile)))

(defn init-version-header-tab [lasfile]
  (init-header-tab (find-header lasfile "VersionHeader")))

(defn init-well-header-tab [lasfile] 
  (init-header-tab (find-header lasfile "WellHeader")))

(defn init-curve-header-tab [lasfile]
  (init-header-tab (find-header lasfile "CurveHeader")))

(defn init-parameter-header-tab [lasfile]
  (init-header-tab (find-header lasfile "ParameterHeader")))

(defn open-headers-editor [lasfile]
  (let [dialog (JDialog. (:frame @app) "Edit Headers")
	panel (JPanel. (MigLayout.))
	pane (JTabbedPane.)]
    (doto pane
      (.addTab "Version" (init-version-header-tab lasfile))
      (.addTab "Well" (init-well-header-tab lasfile))
      (.addTab "Curve" (init-curve-header-tab lasfile))
      (.addTab "Parameter" (init-parameter-header-tab lasfile)))
    (doto panel
      (.add pane "push, grow"))
    (doto dialog
      (.add panel)
      (.setSize (Dimension. 500 600))
      (.setVisible true))
    ))

(defn init-edit-button [lasfile]
  (let [button (JButton. "Edit Headers")]
    (.putClientProperty button "JButton.buttonType" "textured")
    (on-click button
      (fn [e]
	(open-headers-editor lasfile)))
    button))
