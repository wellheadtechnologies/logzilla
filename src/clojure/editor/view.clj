(ns editor.view
  (:use editor.model))

(defn create-depth-slider [editor-data]
  (let [slider (new JSlider 0 (:slider-notches editor-data) 0)]
    (doto slider
      (.setOrientation slider JSlider/VERTICAL))))

(defn create-table [editor-data]
  (let [{:keys [curves]} editor-data
	table (new JTable)
	model (new DefaultTableModel)
	index-data (large-to-small (:data index))]    
    (.addColumn model (:mnemonic index) (into-array Object index-data))
    (doseq [curve (prepare-curves curves)]
      (.addColumn model (:mnemonic curve) (into-array Object (:data curve))))
    (doto table
      (.setModel model))))

(defn create-save-button)