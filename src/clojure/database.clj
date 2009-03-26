(ns database
  (:use util)
  (:import (java.sql Connection DriverManager ResultSet SQLException Statement)
	   (java.io File)))

(Class/forName "org.sqlite.JDBC")

(defstruct LasFile :id :name :headers :index :curves)
(defstruct Header :id :type :prefix :descriptors)
(defstruct Descriptor :mnemonic :unit :data :description)
(defstruct Curve :descriptor :data)
(defstruct Data :id :row :value)

(def connection)

(defn query [string]
  (println "executing query: " string)
  (let [statement (.createStatement connection)]
    (.executeQuery statement string)))

(defmacro with-connection [url & body]
  `(binding [connection (DriverManager/getConnection ~url)]
     (try 
      ~@body
      (finally
       (.close connection)))))

(defn lasfile-primary-key [name]
  (. (query (str "SELECT id FROM lasfiles WHERE name='" name "'"))
     (getInt 1)))

(defn header-descriptors [header-id]
  (let [rs (query (str "SELECT * FROM descriptors WHERE header_id=" header-id))]
    (loop [descriptors []]
      (if (not (.next rs))
	descriptors
	(let [id (.getInt rs 1)
	      mnemonic (.getString rs 2)
	      unit (.getString rs 3)
	      data (.getString rs 4)
	      description (.getString rs 5)]
	  (recur (conj descriptors
		       (struct-map Descriptor
			 :id id
			 :mnemonic mnemonic
			 :unit unit
			 :data data
			 :description description))))))))

(defn lasfile-headers [lf-pk]
  (let [rs (query (str "SELECT id, type, prefix FROM headers WHERE lasfile_id=" lf-pk))]
    (loop [headers []]
      (if (not (.next rs))
	headers
	(let [id (.getInt rs 1)
	      type (.getString rs 2)
	      prefix (.getString rs 3)]
	  (println "header type = " type) 
	  (recur (conj headers 
		       (struct-map Header
			 :id id
			 :type type
			 :prefix prefix
			 :descriptors (header-descriptors id)))))))))

(defn lasfile-curves [curve-header]
  (doall 
   (for [descriptor (:descriptors curve-header)]
     (let [#^ResultSet rs (query (str "SELECT * FROM data WHERE descriptor_id=" (:id descriptor)))]
       (.setFetchSize rs 10000)
       (loop [data []]
	 (if (not (.next rs))
	   data
	   (let [id (.getInt rs 1)
		 row (.getInt rs 2)
		 value (.getDouble rs 3)]
	     (recur (conj data
			  (struct-map Data
			    :id id
			    :row row
			    :value value))))))))))
  
(defn read-lasfile-from-database [name]
  (with-connection "jdbc:sqlite:las.db"
    (let [lf-pk (lasfile-primary-key name)
	  headers (lasfile-headers lf-pk)
	  curve-header (find-first #(= "CurveHeader" (:type %)) headers)
	  curves (lasfile-curves curve-header)]
      (struct-map LasFile
	:id lf-pk
	:name name
	:headers headers
	:curves curves)
      )))

(defn load-lasfile [path]
  (let [file (new File path)
	name (.getName file)
	proc (exec (str  "./lasso -p=" path " -o=database"))]
    (.waitFor proc)
    (read-lasfile-from-database name)))