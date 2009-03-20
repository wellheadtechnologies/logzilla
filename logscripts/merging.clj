(use 'gui.app 'gui.files 'gui.las
     'gui.curves 'gui.global 'gui.util
     'util)

(async-run-main)
(synchronous
 (let [[a b] (open-files ["las_files/test.las"
			  "las_files/test2.las"])]
   (open-las-view a)))