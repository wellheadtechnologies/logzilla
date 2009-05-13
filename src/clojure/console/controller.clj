(ns console.controller
  (:use global util gutil console.text)
  (:import (javax.swing JFrame JPanel JScrollPane JTextArea JTextPane)
	   (net.miginfocom.swing MigLayout)
	   (java.awt Dimension Font Insets)
	   (java.awt.event InputEvent KeyEvent)))

(defn open-console []
  (let [frame (JFrame.)
	text-pane (init-text-pane)
	panel (JPanel. (MigLayout. "ins 0"))
	pane (JScrollPane. panel)]
    (doto panel
      (.add (:widget @text-pane) "push, grow"))
    (doto frame
      (.add pane)
      (.setSize (Dimension. 200 300))
      (.setVisible true))))


;(defstruct Console
;  :ignore
;  :line-start 
;  :history
;  :started-line
;  :hist-line
;  :text
;  :widget)
;
;(defn init-frame [pane]
;  (doto (JFrame.)
;    (.add pane)
;    (.setSize (Dimension. 200 300))
;    (.setVisible true)))
;
;(defn init-console []
;  (let [text (JTextPane.)
;	pane (JScrollPane. text)
;	frame (init-frame pane)
;	font (Font. "Monospaced" Font/PLAIN 14)
;	c (ref (struct-map Console
;			   :ignore false
;			   :line-start 0
;			   :history []		  
;			   :started-line ""
;			   :hist-line 0
;			   :text text
;			   :widget frame))]
;    (doto text
;      (.setText "")
;      (.setFont font)
;      (.setMargin (Insets. 7 5 7 5))
;      (.addKeyListener (init-key-listener c)))
;    (ns user)
;    (require ['sources.controller :as 'sources]
;	     ['editor.controller :as 'editor])
;    (use 'util 'gutil 'global)
;    (enable-interaction)
;    (console-print c ">>")))
;
;(defn console-print [c string]
;  (let [{:keys [text line-start]} @c]
;    (swing-mutator!
;     (console-append c (String/valueOf string))
;     (.setCaretPosition text line-start))))
;
;(defn console-append [c string]
;  (let [text (:text @c)
;	slen (.getCaretPosition text)]
;    (doto text
;      (.select slen slen)
;      (.replaceSelection string))))
;
;(defn control-pressed [e]
;  (> (bit-and (.getModifiers e) InputEvent/CTRL_MASK) 0))
;
;(defn ignore-next [console]
;  (dosync (alter console assoc :ignore true)))
;
;(defn key-press [console e]
;  (let [key-code (.getKeyCode e)
;	{:keys [text line-start]} @console]
;    (cond 
;     (= key-code KeyEvent/VK_UP) 
;     (do 
;       (history-up console)
;       (.consume e))
;     
;     (and (= key-code KeyEvent/VK_A) 
;	  (control-pressed e))
;     (do
;       (ignore-next console)
;       (force-caret-move-to-start console)
;       (.consume e))
;     
;     (and (= key-code KeyEvent/VK_K)
;	  (control-pressed e))
;     (do
;       (doto console
;	 (ignore-next)
;	 (replace-range "" (.getCaretPosition text) (text-length text)))
;       (.consume e))
;     
;     (= key-code KeyEvent/VK_DOWN)
;     (do
;       (history-down console)
;       (.consume e))
;     
;     (or (= key-code KeyEvent/VK_LEFT)
;	 (= key-code KeyEvent/VK_BACK_SPACE))
;     (do
;       (ignore-next console)
;       (when (<= (.getCaretPosition text) line-start)
;	 (.consume e)))
;     
;     (= key-code KeyEvent/VK_DELETE)
;     (ignore-next console)
;     
;     (= key-code KeyEvent/VK_RIGHT)
;     (ignore-next console)
;     
;     (= key-code KeyEvent/VK_ENTER)
;     (let [text (:text @console)]
;       (doto console
;	 (ignore-next)
;	 (enter)
;	 (reset-command-start))
;       (.setCaretPosition text line-start)
;       (.consume e)
;       (.repaint text)))))
;
;(defn key-type [console e]
;  (let [key-char (.getKeyChar e)
;	ignore (:ignore @console)]
;    (cond
;     ignore
;     (done-ignoring console)
;     
;     (any-modifiers e)
;     (console-append console (str key-char)))
;    (.consume e)))
;
;(defn init-key-listener [console]
;  (proxy [KeyListener] []
;    (keyPressed [e] (key-press console e))
;    (keyTyped [e] (key-type console e))
;    (keyReleased [e] (.consume e))))
;
;(defn reset-command-start [console]
;  (dosync
;   (alter console assoc :line-start (text-length console))))
;
;(defn console-append [string]
;  (let [slen (.getCaretPosition text)]
;    (doto text
;      (.select slen slen)
;      (.replaceSelection string))))
;
;(defn replace-range [s start end]
;  (let [st (str s)]
;    (doto (:text @console)
;      (.select start end)
;      (.replaceSelection st))
;    st))
;
;(defn force-caret-move-to-end [console]
;  ())
;
;(defn open-console [] (init-console))
;
;(defn history-up [c]
;  (dosync 
;   (let [{:keys [history hist-line]} @c]
;     (cond
;      (= 0 (.size history)) 
;      nil
;     
;      (= hist-line 0)
;      (alter c assoc :started-line (get-cmd c))
;
;      (< hist-line (.size history))
;      (do
;	(alter c assoc :hist-line (inc hist-line))
;	(once (show-history-line c)))
;      ))))
;
;(defn history-down [c]
;  (dosync
;   (let [{:keys [hist-line]} @c]
;     (cond 
;      (= hist-line 0) nil
;      :else
;      (do 
;	(alter c assoc :hist-line (dec hist-line))
;	(once (show-history-line c)))))))
;
;(defn show-history-line [c]
;  (let [{:keys [started-line history text line-start] @c}
;	show-line (if (= hist-line 0)
;		    started-line
;		    (.get history (- (.size history) hist-line)))]
;    (swing-mutator!
;     (replace-range show-line line-start (text-length text))
;     (doto text
;       (.setCaretPosition (text-length text))
;       (.repaint)))))
;
;(defn text-length [text]
;  (.. text (getDocument) (getLength)))
;
;(defn enter [console]
;  (let [s (get-cmd console)
;	s (if (= 0 (.length s))
;	    ";\n"
;	    (do
;	      (.add history s)
;	      (str s "\n")))]
;    (append console "\n")
;    (dosync (alter console assoc :hist-line 0))
;    (accept-line console s)
;    (.repaint (:text @console))))
;
;(defn get-cmd [console]
;  (let [{:keys [line-start text]}]
;    (.getText (:text @console) line-start (- (text-length text) line-start))))
