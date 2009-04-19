(ns semantics
  (:use util))

(defn well-header [lasfile]
  (let [wh @(find-first #(= "WellHeader" (:type (deref %))) (:headers lasfile))]
    (guard (not (nil? wh))
	   (str "Could not find well header in " (:name lasfile)))
    wh))

(defn find-descriptor-by-name [name descriptors]
  (let [descriptor (find-first #(= name (:mnemonic %)) descriptors)]
    (or descriptor {})))

(defn get-semantics [lasfile]
  (let [wh (well-header lasfile)
	ds (:descriptors wh)
	dfind #(find-descriptor-by-name % ds)]
    {:name (:name lasfile)
     :location (dfind "LOC")
     :depth-start (dfind "STRT")
     :depth-end (dfind "STOP")
     :company (dfind "COMP")
     :well (dfind "WELL")
     :field (dfind "FLD")
     :province-state (dfind "PROV")
     :county (dfind "COUNTY")
     :country (dfind "COUNTRY")
     :well-id (dfind "UWI")
     }))