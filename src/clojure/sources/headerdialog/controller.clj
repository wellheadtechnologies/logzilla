(ns sources.headerdialog.controller
  (:use util gutil global)
  (:import (javax.swing JTable JScrollPane JButton JPanel JDialog JTabbedPane)
	   (org.jdesktop.swingx JXTable)
	   (javax.swing.event TableModelListener)
	   (javax.swing.table DefaultTableModel)
	   (org.jdesktop.swingx.decorator HighlighterFactory)
	   (java.awt Dimension)
	   (net.miginfocom.swing MigLayout)))

(defn save-header [header table]
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
	       :description description})))))

(defn init-table-listener [table header]
  (proxy [TableModelListener] []
      (tableChanged [e] (save-header header table))))

(defswing init-header-tab :mutator [header]
  (let [model (DefaultTableModel.)
	table (JXTable. model)
	pane (JScrollPane. table)
	panel (JPanel. (MigLayout.))
	descriptors (:descriptors @header)]
    (doto table
      (.setSelectionModel (single-selection-model))
      (.addHighlighter (HighlighterFactory/createSimpleStriping)))
    (doto model
      (.addColumn "mnemonic" (into-array Object (map :mnemonic descriptors)))
      (.addColumn "unit" (into-array Object (map :unit descriptors)))
      (.addColumn "data" (into-array Object (map :data descriptors)))
      (.addColumn "description" (into-array Object (map :description descriptors)))
      (.addTableModelListener (init-table-listener table header)))
    (doto panel
      (.add pane "push, grow, wrap"))))

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
  (let [dialog (JDialog. (:sources-frame @app) "Edit Headers")
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
