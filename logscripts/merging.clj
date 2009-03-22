(require 'gui.app
	 'core.files
	 'gui.curves)

(use 'gui.global)

(gui.app/async-run-main)
(synchronous
 (gui-mode
  (let [[a b] (core.files/open-files
	       ["las_files/test.las"
		"las_files/test2.las"])]
    (gui.curves/open-curve-editor a
     [(core.las/get-curve "Facies" (.getCurves  a))
      (core.las/get-curve "Facies" (.getCurves  b))])
    )))