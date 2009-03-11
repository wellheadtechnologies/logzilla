(ns las
  (:use util))

(defn get-curve [lasfile name]
  (let [curves (:las-curves lasfile)]
    (find-first #(= name (:mnemonic %)) curves)))

(defn get-descriptor [header name]
  (let [ds (:descriptors header)]
    (find-first #(= name (:mnemonic %)) ds)))