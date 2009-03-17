(ns timage
  (:use util)
  (:import (core DefaultLasParser DefaultLasWriter)
	   (gui ChartUtil)))

(defn test-slow-rendering-speed []
  (let [lf (DefaultLasParser/parseLasFile "las_files/test.las")]
    (time
     (doseq [c (.getCurves lf)]
       (ChartUtil/curveToIcon c)))))

(defn test-fast-rendering-speed []
  (let [lf (DefaultLasParser/parseLasFile "las_files/test.las")]
    (time 
     (doseq [c (.getCurves lf)]
       (ChartUtil/fastCurveToIcon c)))))

(defn run-tests []
  (test-fast-rendering-speed)

  (println "testing fast rendering speed")
  (test-fast-rendering-speed)
  (println)

  (println "testing slow rendering speed")
  (test-slow-rendering-speed)
  (println)
  )
    