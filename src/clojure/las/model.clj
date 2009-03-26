(ns las.model
  (:use util gutil global))

(defn get-curve [name curves]
  (find-first #(= (:mnemonic %) name) curves))

(defn large-to-small [coll]
  (reverse (sort coll)))

(defn small-to-large [coll]
  (sort coll))


