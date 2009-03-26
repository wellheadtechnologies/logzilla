(ns las.view
  (:use util gutil global)
  (:import (gui IconListCellRenderer)
	   (java.io File)
	   (javax.swing JList JFrame DefaultListModel ImageIcon JLabel
			JScrollPane JButton JWindow JPanel SwingUtilities
			JTabbedPane BorderFactory)
	   (javax.swing.border BevelBorder)
	   (javax.imageio ImageIO)
	   (net.miginfocom.swing MigLayout)
	   (java.awt Dimension Image Color)
	   (java.awt.event MouseMotionAdapter MouseAdapter MouseEvent)))

(def las-panel)
(def edit-action)
(def copy-action)
(def paste-action)
(def remove-action)
(def add-lasfile)
(def jlist-click-action)

(defn create-curve-panel []
  (let [panel (new JPanel (new MigLayout))]
    (doto panel
      (.setBorder (BorderFactory/createEtchedBorder)))
    panel))

(defn create-context-menu [c x y]
  (context-menu [c x y]
    ["Edit" edit-action]
    ["Copy" copy-action]
    ["Paste" paste-action]
    ["Remove" remove-action]))

(defn create-curve-list [lasfile]
  (let [curves (.getCurves lasfile)
	jlist (create-jlist)]
    (add-lasfile lasfile)
    (swing 
     (doto jlist
       (.setFixedCellHeight 80)
       (.setOpaque false))
     (on-click jlist jlist-click-action))
    jlist))

(defn create-las-view [lasfile]
  (let [curve-list (create-curve-list lasfile)
	inner-panel (create-inner-panel)
	pane (new JScrollPane inner-panel)
	outer-panel (create-curve-panel)]
    
    (doto inner-panel
      (.add curve-list "pushx, growx, pushy, growy, wrap"))

    (doto outer-panel 
      (.add pane "pushx, pushy, growx, growy, wrap")
      (.setPreferredSize (new Dimension 400 700)))
    outer-panel))