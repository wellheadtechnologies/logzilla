(use 'gui.app 'gui.files 'gui.las
     'core.las
     'gui.curves 'gui.global 'gui.util
     'util)

(async-run-main)
(synchronous
 (let [[a] (open-files ["las_files/Nat_AMOCO_4.las"])]
   (open-curve-editor (.getCurves a))
   ))