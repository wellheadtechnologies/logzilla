(ns file.headerdialog.controller
  (:use util gutil global)
  (:import (javax.swing JTable JScrollPane JButton JPanel JDialog JTabbedPane)
	   (org.jdesktop.swingx JXTable)
	   (javax.swing.table DefaultTableModel)
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn init-save-button [header table]
  (let [button (JButton. "Save Changes")]
    (on-click button
      (fn [e]
	(dosync 
	 (alter header assoc :descriptors
		(for [row (range 0 (count (:descriptors @header)))]
		  (let [mnemonic (.getValueAt table row 0)
			unit (.getValueAt table row 1)
			data (.getValueAt table row 2)
			description (.getValueAt table row 3)]
		    {:mnemonic mnemonic
		     :unit unit
		     :data data
		     :description description}))))))
    button))

(defn init-header-tab [header]
  (swing-io!
   (let [model (DefaultTableModel.)
	 table (JXTable. model)
	 pane (JScrollPane. table)
	 panel (JPanel. (MigLayout.))
	 save-button (init-save-button header table)
	 descriptors (:descriptors @header)]
     (doto model
       (.addColumn "mnemonic" (into-array Object (map :mnemonic descriptors)))
       (.addColumn "unit" (into-array Object (map :unit descriptors)))
       (.addColumn "data" (into-array Object (map :data descriptors)))
       (.addColumn "description" (into-array Object (map :description descriptors))))
     (doto panel
       (.add pane "push, grow, wrap")
       (.add save-button "align 50%")))))

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
      (.setVisible true))))

(defn init-edit-button [lasfile]
  (let [button (JButton. "Edit Headers")]
    (.putClientProperty button "JButton.buttonType" "textured")
    (on-click button
      (fn [e]
	(open-headers-editor lasfile)))
    button))
