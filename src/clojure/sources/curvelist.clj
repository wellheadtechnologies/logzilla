(ns sources.controller)

(declare make-transferable open-curve-editor-action init-inspector-listener)

(defn create-curve-list []
  (let [jlist (JList. (DefaultListModel.))
	dragger (proxy [Dragger] [jlist]
		  (createTransferable [c]
				      (make-transferable (first (.getSelectedValues c))))
		  (createIcon [c] (.. (first (.getSelectedValues c)) (getIcon) (getImage))))]
    (doto jlist
      (.setVisibleRowCount 0)
      (.setBorder (BorderFactory/createEmptyBorder))
      (.setCellRenderer (IconListCellRenderer.))
      (.setBackground (.getBackground (JPanel.)))
      (.setOpaque false))))

(defn create-curve-list-view [curve-list]
  (let [inner-panel (JPanel. (MigLayout. "ins 0"))
	pane (JScrollPane. inner-panel)
	outer-panel (JPanel. (MigLayout. "ins 0"))]
    (doto inner-panel
      (.add curve-list "pushx, growx, pushy, growy, wrap"))
    (doto outer-panel
      (.add pane "pushx, pushy, growx, growy, wrap"))))

(defn add-curve-to-gui [curve-list curve]
  (let [icon (chart.render/curve-to-icon curve)]
    (dosync 
     (alter curve assoc :icon icon)
     (swing 
      (.addElement (.getModel curve-list) icon)
      (.repaint curve-list)))))

(defn add-curve [file curve]
  (dosync 
   (let [lasfile (:lasfile @file)
	 curves (:curves @lasfile)
	 curve-list (:curve-list @file)]
     (alter lasfile assoc :curves (conj curves curve))
     (long-task (add-curve-to-gui curve-list curve)))))

(defn init-curve-list [source-manager curves]
  (let [curve-list (create-curve-list)]
    (long-task
      (doseq [curve curves]
	(add-curve-to-gui curve-list curve)))
    (doto curve-list
      (.addMouseListener (click-listener (partial open-curve-editor-action source-manager)))
      (.addMouseListener (init-context-menu-listener source-manager curve-list))
      (.addListSelectionListener (init-inspector-listener curve-list)))
    curve-list))

(defn init-curve-list-view [curve-list]
  (create-curve-list-view curve-list))

(defn make-transferable [curve]
  (proxy [Transferable] []
    (getTransferData [flavor] curve)
    (getTransferDataFlavors [] 
			    (let [flavors (into-array DataFlavor [ref-data-flavor])]
			      flavors))
    (isDataFlavorSupported [flavor] false)))

(defn create-transfer-handler []
  (proxy [CustomTransferHandler] []
    (createTransferable [c] 
			(make-transferable (first (.getSelectedValues c))))
    (getSourceActions [c] TransferHandler/COPY)))

(defn create-curve-panel []
  (let [panel (JPanel. (MigLayout. "ins 0"))]
    (doto panel
      (.setBorder (BorderFactory/createBevelBorder BevelBorder/LOWERED)))))

(defn update-curve-icon [curve-list old-descriptor curve]
  (dosync 
   (let [icon (:icon @curve)
	 descriptor (:descriptor @curve)]
     (when (not= descriptor old-descriptor)
       (swing 
	(.setText icon (:mnemonic descriptor))
	(.repaint curve-list)))
     descriptor)))

(defn get-selected-curves [curve-list]
  (swing-io! (doall (map #(.getCurve %) (.getSelectedValues curve-list)))))

