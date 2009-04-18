(ns editor.view
  (:use editor.model util gutil global)
  (:import (javax.swing JButton JSlider JTable JToggleButton ImageIcon)
	   (javax.swing.table DefaultTableModel)))

(defn create-save-button [save-action]
  (let [button (JButton. (ImageIcon. "resources/save.png"))]
    (.putClientProperty button "JButton.buttonType" "textured")
    (on-action button (save-action))
    button))

(defn create-edit-button [edit-action]
  (let [button (JToggleButton. (ImageIcon. "resources/edit.png"))]
    (.putClientProperty button "JButton.buttonType" "textured")
    (on-action button (edit-action))
    button))

(defn create-zoom-button [zoom-action]
  (let [button (JToggleButton. (ImageIcon. "resources/zoom.png"))]
    (.putClientProperty button "JButton.buttonType" "textured")
    (on-action button (zoom-action))
    button))

(defn create-reset-button [reset-action]
  (let [button (JButton. "Reset Scale")]
    (.putClientProperty button "JButton.buttonType" "textured")
    (on-action button (reset-action))
    button))