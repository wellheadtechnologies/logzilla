(use 'gui.app 'gui.files 'gui.las
     'gui.curves 'gui.global 'gui.util
     'util)

(async-run-main)
(synchronous
 (let [[t d] (open-files ["las_files/test.las" "las_files/dollie.las"])]
   (open-las-view t)))

