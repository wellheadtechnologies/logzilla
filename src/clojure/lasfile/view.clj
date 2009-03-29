(ns lasfile.view
  (:use gutil)
  (:import (javax.swing JMenu JFileChooser JPanel 
			JScrollPane JList DefaultListModel
			BorderFactory JTabbedPane)
	   (gui IconListCellRenderer)
	   (net.miginfocom.swing MigLayout)))

(defstruct FileMenuConfig :open-action :save-all-action :quit-action)
(defstruct LasViewConfig :las-file :curve-list)
(defstruct ContextMenuConfig
  :component :x :y
  :edit-action
  :copy-action
  :paste-action
  :remove-action)

(def file-menu-config (agent {})) ;FileMenuConfig

(defn create-file-menu [{:keys [open-action save-all-action quit-action] :as file-menu-config}]
  (let [menu (new JMenu "Las")]
    (actions menu
      ["Open" open-action]
      ["Save All" save-all-action]
      ["Quit" quit-action])
    menu))

(defn create-file-selection-dialog [cwd]
  (let [chooser (new JFileChooser cwd)]
    (.setMultiSelectionEnabled chooser true)
    chooser))

(defn create-curve-panel []
  (let [panel (new JPanel (new MigLayout))]
    (doto panel
      (.setBorder (BorderFactory/createEtchedBorder)))))

(defn create-context-menu [{:keys [component x y] :as config}]
  (context-menu [component x y]
    ["Edit" (:edit-action config)]
    ["Copy" (:copy-action config)]
    ["Paste" (:paste-action config)]
    ["Remove" (:remove-action config)]))

(defn create-curve-list [click-action]
  (let [jlist (new JList (new DefaultListModel))]
    (on-click jlist click-action)
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