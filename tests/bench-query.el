(require 'nov)
(require 'epub)


(defun bench1 (num-iters fpath)
  "Benchmark the speed of retrieving the url of the package document from 'container.xml', run each function NUM-ITER times.
FPATH need to be a vaid path for a 'container.xml' file."
  (let* ((pt (epub-parse-xml fpath))
	 (dur-nov
	  (benchmark-run num-iters
	    (nov-container-content-filename pt)))
	 (dur-exml
	  (benchmark-run num-iters
	    (dom-attr (exml-find '("*" nil (rootfile nil)) pt)
		      'full-path))))
    (message
     (format "\n------\nRetrieving package document from %s:" fpath))
    (message (format "runtime using nov:  %s" (car dur-nov)))
    (message (format "runtime using exml: %s" (car dur-exml)))
    (message "------\n")
    ())
  )

;; Free epub samples from Project Gutenberg (https://www.gutenberg.org/)
;; are included under the folder "<git-dir>/tests/epubs/".
;; Files under directory "<git-dir>/tests/containers/" are unzipped versions
;; of the epub files which can be used for running the benchmarks.

;; (bench1 5 "./containers/pg25344-images-3/META-INF/container.xml")
;; (bench1 5 "./containers/pg1342-images-3/META-INF/container.xml")
;; output:
;; ------
;; Retrieving package document from./containers/pg1342-images-3/META-INF/container.xml:
;; runtime using nov:  5.20593773
;; runtime using exml: 0.000170321
;; ------

(defun bench2 (num-iters fpath)
  "Benchmark the speed of retrieving all <a> tags from an '.xhtml' content file, run each function NUM-ITER times.
FPATH need to be a vaid path for an '.xhtml' file."
  (let* ((pt (epub-parse-xml fpath))
	 (dur-nov
	  (benchmark-run num-iters
	    (esxml-query-all "a" pt)))
	 (dur-exml
	  (benchmark-run num-iters
	    (exml-findall '("*" nil (a nil)) pt)))
	 )
    ;; (insert (format "%s\n" (esxml-query-all "a" pt)))
    ;; (insert (format "%s\n" (exml-findall '("*" nil (a nil)) pt)))
    (message
     (format "\n------\nRetrieving <a> tags from %s:" fpath))
    (message (format "runtime using nov:  %s" (car dur-nov)))
    (message (format "runtime using exml: %s" (car dur-exml)))
    (message "------\n")
    ()
    )
  )

;; (bench2 5 "./containers/pg1342-images-3/OEBPS/toc.xhtml")
;; (bench2 5 "./containers/pg25344-images-3/OEBPS/toc.xhtml")

(defun bench3 (num-iters fpath)
  "Benchmark the speed of retrieving all <text> tags from an '.ncx' toc file, run each function NUM-ITER times.
FPATH need to be a vaid path for an '.ncx' file."
  (let* ((pt (epub-parse-xml fpath))
	 (dur-nov
	  (benchmark-run num-iters
	    (esxml-query-all "navLabel>text" pt)))
	 (dur-exml
	  (benchmark-run num-iters
	    (exml-findall '("*" nil (navLabel nil (text nil)))
			  pt)))
	 )
    ;; (insert (format "%s\n" (esxml-query-all "navLabel>text" pt)))
    ;; (insert (format "%s\n" (exml-findall '("*" nil (navLabel nil (text nil)))
    ;; 					 pt)))
    (message
     (format "\n------\nRetrieving <text> tags from %s:" fpath))
    (message (format "runtime using nov:  %s" (car dur-nov)))
    (message (format "runtime using exml: %s" (car dur-exml)))
    (message "------\n")
    ()
    )
  )

;; (bench3 5 "./containers/pg1342-images-3/OEBPS/toc.ncx")
;; output:
;; ------
;; Retrieving <text> tags from ./containers/pg1342-images-3/OEBPS/toc.ncx:
;; runtime using nov:  1.558169264
;; runtime using exml: 0.37223006200000003
;; ------
