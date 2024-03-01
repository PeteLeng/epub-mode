;; -*- lexical-binding: t -*-

(defun node-tag (node)
  (dom-tag node))

(defun node-children (node)
  (and (consp node) (cddr node)))

(defun node-attrs (node)
  (and (consp node) (cadr node)))

(defun node-attr (attr node)
  (and (consp node)
       (cdr (assoc attr (nth 1 node)))))

(defun visit-node (func node)
  (funcall #'func node)
  (mapc (apply-partially #'visit-node func) (node-children node)))

;; (defun match-node (node patt)
;;   (or (null patt)
;;       (and node
;; 	   (equal (node-tag node) (node-tag patt))
;; 	   (let ((nodes (node-children node))
;; 		 (patts (node-children patt)))
;; 	     (and (>= (length nodes) (length patts))
;; 		  (seq-reduce #'(lambda (acc v) (and acc v))
;; 			      (cl-mapcar #'match-node nodes patts)
;; 			      t))))))

(defun match-tag (n1 n2)
  "Match the tag of two exml nodes"
  (equal (node-tag n1) (node-tag n2)))

(defun match-attr (pattr node)
  "Match an attribute pattern against a exml node
Each attribute pattern is a list of (op attr-name attr-val)"
  (or (null pattr)
      (let* ((attrs (dom-attributes node))
	     (pname (cadr pattr))
	     (aval (cdr (assoc pname attrs))))
	(and aval
	     (pcase pattr
	       (`("=" ,_ ,pval) (string-equal pval aval))
	       (`("*=" ,_ ,pval) (string-match pval aval))
	       (`("^=" ,_ ,pval) (string-match (concat "^" pval) aval))
	       (`("$=" ,_ ,pval) (string-match (concat pval "$") aval))
	       (`(,op . ,_) (error "operator not implemented"))
	       )
	     t))))

(defun match-attrs (pattrs node)
  "Match a list of attribute pattens against a node"
  (or (null pattrs)
      (seq-every-p (lambda (pattr)
		     (match-attr pattr node))
		   pattrs)))

(defun match-patt (patt root)
  ;; patt is a node and matches from root
  (and (consp root) ;; root cannot be a leaf node
       (pcase patt
	 (`() (list root)) ;; return root for empty pattern
	 (`("*" . ,_) (match-wild patt root))
	 (`(,ptag ,pattrs)
	  (and (equal ptag (node-tag root)) ;; match pattern without children
	       (match-attrs pattrs root)
	       (list root)))
	 (`(,ptag ,pattrs . ,pchildren)
	  (and (equal ptag (node-tag root))
	       (match-attrs pattrs root)
	       (match-patts pchildren
			    (node-children root))))
	 (_ (error "malformed pattern: %s" patt))))
  )

(defun match-wild (wpat root)
  ;; wildcard pattern is a pattern that starts with "*"
  ;; walk down node until it potentially matches the next pattern
  (pcase wpat
    (`("*" ,_)
     (cons root
	   (mapcan (apply-partially #'match-wild wpat)
		   (node-children root))))
    (`("*" ,_ ,next)
     (nconc (match-patt next root) ;; try skip *
	    (mapcan (apply-partially #'match-wild wpat)
		    (node-children root))))
    (`("*" ,_ . ,children)
     (nconc (mapcan (apply-partially #'match-wild wpat)
		    (node-children root))
	    (cl-mapcon (apply-partially #'match-patts children)
		       (node-children root)))))
  )

(defun match-patts (patts nodes)
  ;; (message (format "match patts p: %s with n: %s" patts nodes))
  (and nodes
       (pcase patts
	 (`() (error "empty pat list"))
	 (`(("*" . ,_) . ,_) (match-wilds patts nodes))
	 (`(,patt) (match-patt patt (car nodes)))
	 (`(,patt . ,rest) (and (match-patt patt (car nodes))
				(match-patts rest (cdr nodes))))))
  )

(defun match-wilds (patts nodes)
  ;; (message (format "match wilds p: %s with n: %s" patts nodes))
  (and nodes
       (pcase patts
	 ;; not strictly necessary, but more efficient.
	 (`(("*" ,_))
	  (mapcan (apply-partially #'match-wild (car patts))
		  nodes))
	 (`(("*" ,_ . ,pchildren))
	  (when-let ((matches (match-wild (car patts) (car nodes))))
	    (nconc matches (match-wilds patts (cdr nodes)))
	    ))
	 (`(("*" ,_) . ,rest)
	  (nconc (match-patts rest nodes) ;; try skip *
		 (match-wilds patts (cdr nodes))))
	 (`(("*" ,_ . ,pchilren) . ,rest)
	  (and (match-wild (car patts) (car nodes)) ;; match the first node
	       (nconc (match-wilds patts (cdr nodes))
		      (match-patts rest (cdr nodes)))
	       ))))
  )

(defun exml-find (patt node)
  (car (match-patt patt node)))

(defun exml-findall (patt node)
  (match-patt patt node))

;; test
(defun exml-test ()
  (let ((node '(root nil (c1 nil (c11 nil) (c12 nil (c1 nil))) (c1 nil) (c3 nil)))
	(pats '(
		(root nil)
		(root nil (c1 nil))
		(root nil (c1 nil (c11 nil)))
		(root nil (c1 nil) (c2 nil))
		(root nil (c1 nil (c11 nil)) (c2 nil))
		(root nil (c1 nil (c11 nil)) (c2 nil) (c3 nil))
		(root nil (c1 nil (c11 nil) (c12 nil) (c13 nil)))
		("*" nil)
		("*" nil (c1 nil))
		("*" nil (c1 nil (c11 nil)))
		("*" nil (c1 nil ("*" nil) (c12 nil)))
		(root nil ("*" nil) (c1 nil))
		(root nil ("*" nil (c12 nil)))
		(root nil ("*" nil))
		(root nil ("*" nil (c12 nil)) (c1 nil))
		("*" nil (c2 nil))
		("*" nil (c12 nil))
		)))
    (insert (format "match node: %s\n" node))
    (dolist (pat pats)
      (insert (format "with pattern: %s\n" pat))
      (insert (format "%s\n" (match-patt pat node)))))
  )

;; (iter-defun range (n)
;;   (while (> n 0)
;;     (iter-yield n)
;;     (setq n (1- n))))

(provide 'exml-query)
