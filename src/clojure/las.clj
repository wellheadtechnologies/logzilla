(ns las
  (:use util))

(defn get-curve [lasfile name]
  (let [curves (:curves lasfile)]
    (find-first #(= name (:mnemonic %)) curves)))

(defn find-curve [curves name]
  (find-first #(= name (:mnemonic %)) curves))

(defn add-curve [lasfile curve]
  (let [pre-existing (:curves lasfile)]
    (assoc lasfile :curves (conj pre-existing curve))))

(defn get-descriptor [header name]
  (let [ds (:descriptors header)]
    (find-first #(= name (:mnemonic %)) ds)))

(defn cmin [curve]
  (first (:data curve)))

(defn cmax [curve]
  (last (:data curve)))
