(ns registry
  (:use util global gutil)
  (:import (javax.swing JFrame)))

(println "deflogger = " (macroexpand '(deflogger registry)))
(deflogger registry)

(def properties (ref {}))
(def registered-objects (ref {}))

(defn define [type props]
  (debug (str "defining " type " as " props))
  (dosync
   (alter properties assoc type props)))

(defn register [type obj]
  (debug (str "registering (" (truncate (str obj)) ") in " type))
  (dosync
   (let [old-objs (get @registered-objects type)]
     (guard (not (some #(= obj %) old-objs)) (str "cannot register an already registered object (" (truncate (str obj)) ")"))
     (alter registered-objects assoc type (conj old-objs obj))))
  obj)

(defn lookup 
  ([type] (get @registered-objects type))
  ([type pred] 
     (filter pred (get @registered-objects type))))

(defn unregister [type obj]
  (debug (str "unregistering (" (truncate (str obj)) ") from " type))
  (dosync
   (let [old-objs (get @registered-objects type)]
     (guard (some #(= obj %) old-objs) (str "cannot unregister an already unregistered object (" (truncate (str obj)) ")"))
     (alter registered-objects assoc type (remove #(= obj %) old-objs))))
  obj)

(defn acquire-registered-frame []
  (let [frame (JFrame.)]
    (register :frames frame)
    frame))

(defn dispose-registered-frame [frame]
  (unregister :frames frame)
  (swing-agent
   (doto frame
     (.setVisible false)
     (.dispose))))

(defn dispose-frames []
  (dosync
   (doseq [frame (lookup :frames)]
     (dispose-registered-frame frame))))