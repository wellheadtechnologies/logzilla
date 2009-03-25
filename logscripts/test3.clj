(use 'gui.app 'gui.files 'gui.las
     'gui.curves 'gui.global 'gui.util
     'util)

(async-run-main)
(synchronous
 (let [lf (open-file "las_files/test.las")
       curves (.getCurves lf)
       facies (get-curve "Facies" curves)]
   (open-las-view lf)
   (Thread/sleep 2000)
   (remove-curve lf facies)))