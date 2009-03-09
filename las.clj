(ns las
  (:use util))

(defn curve [lasfile name]
  (let [curves (:las-curves lasfile)]
    (find-first #(= name (:mnemonic %)) curves)))