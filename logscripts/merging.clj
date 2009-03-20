(use 'gui.app 'gui.files 'gui.las
     'gui.curves 'gui.global 'gui.util
     'util)

(async-run-main)
(synchronous
 (let [[a b c d e f] (open-files ["las_files/Nat_Amoco_A4.las"
				  "las_files/Nat_Amoco_A4run1.las" 
				  "las_files/Nat_Amoco_A4run2.las"
				  "las_files/Nat_AMOCO_4.las"
				  "las_files/Nat_AMOCO_4_run1.las"
				  "las_files/Nat_AMOCO_4_run2.las"])]
   (open-las-view a)))