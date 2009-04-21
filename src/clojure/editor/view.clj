(ns editor.view
  (:use editor.model util gutil global)
  (:import (javax.swing JButton JSlider JTable JToggleButton ImageIcon)
	   (javax.imageio ImageIO)
	   (javax.swing.table DefaultTableModel)
	   (java.io File)
	   (java.awt Image)))

(defn create-save-button [save-action]
  (let [button (JButton. (ImageIcon. "resources/save.png"))]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action (save-action)))))

(defn create-edit-button [edit-action]
  (let [button (JToggleButton. (ImageIcon. "resources/edit.png"))]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action (edit-action)))))

(defn create-zoom-button [zoom-action]
  (let [button (JToggleButton. (ImageIcon. "resources/zoom.png"))]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action (zoom-action)))))

(defn create-reset-button [reset-action]
  (let [button (JButton. "Reset Scale")]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action (reset-action)))))

(defn create-points-button [points-action]
  (let [button (JToggleButton. "Points")]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action (points-action)))))

(defn create-pan-button [pan-action]
  (let [button (JToggleButton. (ImageIcon. glove-image))]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (on-action (pan-action)))))