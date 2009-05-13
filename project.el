(nml-load-clojure)
(setq current-project-directory "/Users/nathan/projects/wellhead/logzilla/")
(setq current-project-lib-directory (concat current-project-directory "lib"))
(swank-clojure-config
 (dolist (file-name (file-name-all-completions "" current-project-lib-directory))
   (add-to-list 'swank-clojure-extra-classpaths
		(expand-file-name file-name current-project-lib-directory)))
 (add-to-list 'swank-clojure-extra-classpaths
	      (expand-file-name "src/clojure/" current-project-directory))
 (add-to-list 'swank-clojure-extra-classpaths
	      (expand-file-name "build/logzilla.jar" current-project-directory)))
(nml-load-slime)

(define-clojure-indent
  (with-input 1)
  (save-excursion 1)
  (actions 1)
  (on-action 1)
  (context-menu 1)
  (on-click 1)
  (button 1)
  (with-connection 1)
  (with-proc-reader 1)
  (with-proc-writer 1)
  (unless 1)
  (wait-for 1)
  (defprobe 1)
  (switch 1)
  (swing 1))