(ns tparser2
  (:use util))

(import '(core DefaultLasParser))

(defn test-las-file []
  (let [lf (DefaultLasParser/parseLasFile "las_files/x4.las")]
    (println lf)))