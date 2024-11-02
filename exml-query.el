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
  "Match the tag of two exml nodes."
  (equal (node-tag n1) (node-tag n2)))

(defun match-attr (patt node)
  "Match an attribute against a exml node,
Each pattern looks like (op name val)."
  (or (null patt)
      (let* ((name (cadr patt))
	     (attrs (dom-attributes node))
	     (val (cdr (assoc name attrs))))
	(and val
	     (pcase patt
	       (`("=" ,_ ,pval) (string-equal pval val))
	       (`("*=" ,_ ,pval) (string-match pval val))
	       (`("^=" ,_ ,pval) (string-match (concat "^" pval) val))
	       (`("$=" ,_ ,pval) (string-match (concat pval "$") val))
	       (`(,op . ,_) (error "operator not implemented"))
	       )
	     t))))

(defun match-attrs (patts node)
  "Match a list of attributes against a node."
  (or (null patts)
      (seq-every-p (lambda (patt)
		     (match-attr patt node))
		   patts)))

;; The main purpose of this package is to retrieve
;; a list of nodes, the destination nodes, nested within
;; the node where the search starts, the source node,
;; where the path from the src to the dst matches the
;; pattern.
;; The intermediate representation for the patterns
;; are node-like structures.

;; Examples of the intermediate representation,
;; using jQuery selectors as comparison,
;; - All Selector ("*")
;; - '("*" nil)
;; - Next Adjacent Selector ("prev + next")
;; - '("*" nil (prev nil) (next nil))
;; - Next Siblings Selector ("prev ~ next")
;; - '("*" nil (prev nil) (* nil) (next nil))
;; ...

;; The patterns are matched against the nodes
;; in a DFS fashion, from top to bottom,
;; and left to right.

(defun match-patt (patt nd)
  "Match node nd against a pattern patt,
return the list of matched node or nodes, in case of wildcard pattern.
patt is also a node using intermediate representation."
  (and (not (null patt)) ;; empty pattern does not make sense.
       (consp nd) ;; nd has to be a list, i.e., not a leaf node.
       (pcase patt
	 (`() ;; should not happen, check regardless
	  (error "empty pattern"))
	 (`("*" . ,_) (match-wc patt nd))
	 (`(,tag ,attrs)
	  (and (equal tag (node-tag nd))
	       (match-attrs attrs nd)
	       (list nd)))
	 (`(,tag ,attrs . ,rest)
	  (and (equal tag (node-tag nd))
	       (match-attrs attrs nd)
	       (match-patts rest (node-children nd))))
	 (_ (error "malformed pattern: %s" patt))))
  )

(defun match-wc (patt nd)
  "Match node nd with a wildcard pattern patt,
return a list of node or nodes matched."
  ;; wildcard pattern starts with "*"
  (pcase patt
    (`("*" ,_)
     (cons nd
	   (mapcan (apply-partially #'match-wc patt)
		   (node-children nd))))
    (`("*" ,_ ,next)
     (nconc (match-patt next nd) ;; skip the wc pattern
	    (mapcan (apply-partially #'match-wc patt)
		    (node-children nd))))
    (`("*" ,_ . ,children)
     ;; traverse the node in DFS fashion
     (nconc (mapcan (apply-partially #'match-wc patt)
		    (node-children nd))
	    (cl-mapcon (apply-partially #'match-patts children)
		       (node-children nd)))))
  )

(defun match-patts (patts nodes)
  "Match a list of nodes against a list of patterns"
  ;; (message (format "match patts p: %s with n: %s" patts nodes))
  (and (not (null patts)) ;; empty patterns do not make sense
       nodes
       (pcase patts
	 (`() ;; not happening, check anyway
	  (error "empty pattern list"))
	 (`(("*" . ,_) . ,_) (match-patts-wc patts nodes))
	 ;; (`(("~" . ,_) . ,rest)) ;; supported by "*"
	 (`(,patt) (match-patt patt (car nodes)))
	 (`(,patt . ,rest) (and (match-patt patt (car nodes))
				(match-patts rest (cdr nodes))))))
  )


(defun match-patts-wc (patts nodes)
  ;; (message (format "match wilds p: %s with n: %s" patts nodes))
  (and nodes
       (pcase patts
	 (`(("*" ,_)) ;; branch not necessary
	  (mapcan (apply-partially #'match-wc '("*" nil))
		  nodes))
	 (`(("*" ,_ . ,_))
	  (mapcan (apply-partially #'match-wc (car patts))
		  nodes))
	 (`(("*" ,_) . ,rest)
	  (nconc (match-patts rest nodes) ;; try skip wc pattern
		 (match-patts-wc patts (cdr nodes))))
	 (`(("*" ,_ . ,chldr) . ,rest)
	  (nconc (and (match-wc (car patts) (car nodes))
		      (match-patts rest (cdr nodes)))
		 (match-patts patts (cdr nodes)))))
       ))

(defun exml-find (patt node)
  (car (match-patt patt node)))

(defun exml-findall (patt node)
  (match-patt patt node))

;; Tests
;; Given the node and the following patterns,

;; a
;; | \
;; |\ \
;; | \ \
;; b  c d
;; | \ 
;; |\ \
;; a c d
;;     |
;;     e

;; '("*" nil (c)) should return two nodes,
;; '("*" nil ("*" nil (c)) (c)) should return one node.

(defun exml-test1 ()
  (let ((nd '(a nil
		(b nil
		   (a nil)
		   (c nil)
		   (d nil
		      (e nil)))
		(c nil)
		(d nil)))
	(patts '(
		 ("*" nil (c nil))
		 ("*" nil ("*" nil (c nil)) (c nil))
		 ("*" nil (c nil) (d nil))
		 ("*" nil ("*" nil (e nil)) (c nil))
		 ("*" nil ("*" nil (e nil)) (d nil))
		 ("*" nil ("*" nil (e nil)) ("*" nil) (d nil))
		 )))
    (insert (format "match node: %s\n" nd))
    (dolist (patt patts)
      (insert (format "with pattern: %s\n" patt))
      (insert (format "%s\n" (match-patt patt nd))))
    ))

;; (exml-test1)

(defun exml-test ()
  (let ((node '(root nil
		     (c1 nil
			 (c11 nil)
			 (c12 nil
			      (c1 nil)))
		     (c1 nil
			 (c12 nil))
		     (c3 nil)))
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
		("*" nil (c3 nil))
		)))
    (insert (format "match node: %s\n" node))
    (dolist (pat pats)
      (insert (format "with pattern: %s\n" pat))
      (insert (format "%s\n" (match-patt pat node)))))
  )

;; (exml-test)

(provide 'exml-query)
