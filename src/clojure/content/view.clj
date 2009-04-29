(ns content.view
  (:import (javax.swing JPanel JTabbedPane JLabel)
	   (java.awt Color Dimension)
	   (net.miginfocom.swing MigLayout)
	   (org.netbeans.swing.tabcontrol TabbedContainer DefaultTabDataModel TabData)))

(def model (DefaultTabDataModel.))
(def container (TabbedContainer. model 3))
(def tab-data1 (TabData. (JLabel. "depth") nil "test.las" "blah"))
(def tab-data2 (TabData. (JLabel. "facies") nil "facies.las" "blah2"))

(doto model
  (.addTabs 0 (into-array TabData [tab-data1 tab-data2])))

(defn create-content-widget []
  (let [panel (JPanel. (MigLayout. "ins 0"))
	tab-pane (JTabbedPane.)]
    (doto panel 
      (.add container "push, grow"))))