(use 'gui.app 'gui.files 'gui.las
     'core.las
     'gui.curves 'gui.global 'gui.util
     'util)

(async-run-main)
(synchronous
 (let [[a b] (open-files ["las_files/test.las"
			  "las_files/test2.las"])]
   (open-curve-editor [(get-curve "Facies" (.getCurves  a))
		       (get-curve "Facies" (.getCurves  b))])
))