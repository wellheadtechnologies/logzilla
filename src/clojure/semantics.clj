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
    (assoc (apply hash-map
		  (apply concat
			 (remove nil? 
				 (for [[k v] standard-mapping]
				   (let [descriptor (dfind v)]
				     (when descriptor
				       [k descriptor]))))))
      :name (:name lasfile))))

(defn update-descriptor [lasfile name new-val]
  (dosync
   (let [wh (well-header @lasfile)
	 old-descriptors (:descriptors @wh)
	 mnemonic (name-to-mnemonic name)
	 old-descriptor (find-first #(= mnemonic (:mnemonic %)) old-descriptors)
	 new-descriptor (assoc old-descriptor :data new-val)
	 new-descriptors (replace {old-descriptor new-descriptor} old-descriptors)]
     (alter wh assoc :descriptors new-descriptors))))

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
