(ns semantics
  (:use util))

(declare name-to-mnemonic mnemonic-to-name standard-mapping reverse-standard-mapping)

(defn well-header [lasfile]
  (let [wh (find-first #(= "WellHeader" (:type (deref %))) (:headers lasfile))]
    (guard (not (nil? wh))
	   (str "Could not find well header in " (:name lasfile)))
    wh))

(defn find-descriptor-by-name [name descriptors]
  (let [descriptor (find-first #(= name (:mnemonic %)) descriptors)]
    (or descriptor {})))

(defn get-semantics [lasfile]
  (let [wh @(well-header lasfile)
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

(defn update-descriptor [lasfile name new-val]
  (println "update-semantics " name " " new-val)
  (dosync
   (let [wh (well-header @lasfile)
	 old-descriptors (:descriptors @wh)
	 mnemonic (name-to-mnemonic name)
	 old-descriptor (find-first #(= mnemonic (:mnemonic %)) old-descriptors)
	 new-descriptor (assoc old-descriptor :data new-val)
	 new-descriptors (replace {old-descriptor new-descriptor} old-descriptors)]
     (println "old-descriptors = " old-descriptors)
     (println "old-descriptor = " old-descriptor)
     (println "new-descriptor = " new-descriptor)
     (alter wh assoc :descriptors new-descriptors)))
  (println (well-header @lasfile)))

(defn name-to-mnemonic [name]
  (get standard-mapping name))

(defn mnemonic-to-name [mnemonic]
  (get reverse-standard-mapping mnemonic))

(def standard-mapping
     {:location "LOC" 
      :depth-start "STRT"
      :depth-end "STOP"
      :company "COMP"
      :well "WELL"
      :field "FLD"
      :province-state "PROV"
      :county "COUNTY"
      :country "COUNTRY"
      :well-id "UWI"})

(def reverse-standard-mapping (reverse-map standard-mapping))