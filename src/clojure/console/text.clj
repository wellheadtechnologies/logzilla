(ns console.text 
  (:use util gutil global)
  (:import (java.awt Font Insets)
	   (javax.swing JTextPane)))

(defstruct TextPane 
  :widget
  :lines)

(def default-font 
     {:name "Monospaced" 
      :style Font/PLAIN 
      :size 14})

(def default-insets
     {:top 7 
      :left 5
      :bottom 7
      :right 5})

(defn get-font [attributes]
  (Font. (:name attributes) 
	 (:style attributes)
	 (:size attributes)))

(defn get-insets [attributes]
  (Insets. (:top attributes)
	   (:left attributes)
	   (:bottom attributes)
	   (:right attributes)))

(defn init-text-pane []
  (let [widget (JTextPane. )
	font (get-font default-font)
	insets (get-insets default-insets)
	text-pane (ref 
		   (struct-map TextPane
		     :widget widget
		     :lines []))]
    (doto widget
      (.setText "")
      (.setFont font)
      (.setMargin insets))
    text-pane))

(defn text-length [text-pane]
  (swing-getter
   (let [widget (:widget @text-pane)]
     (.. widget (getDocument) (getLength)))))

(defn replace-last-line [text-pane obj]
  (dosync
   (let [{:keys [widget lines]} @text-pane
	 last-line (last lines)
	 start (:end last-line)
	 end (text-length text-pane)
	 line-value (str obj "\n")
	 line-start start
	 line-end (count line-value)
	 line {:start line-start :end line-end :value line-value}]
     (alter text-pane assoc :lines (conj (drop-last lines) line))
     (swing-agent
      (doto widget
	(.select start end)
	(.replaceSelection line-value))))))

(defn text-println [text-pane obj]
  (dosync
   (let [{:keys [widget lines]} @text-pane
	 line-value (str obj "\n")
	 line-start (text-length text-pane)
	 line-end (+ line-start (count line-value))
	 line {:start line-start :end line-end :value line-value}]
     (alter text-pane assoc :lines (conj lines line))
     (swing-agent
      (let [slen (text-length text-pane)]
	(doto widget
	  (.select slen slen)
	  (.replaceSelection line-value)))))))
