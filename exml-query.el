;; -*- lexical-binding: t -*-
;;; exml-query.el --- An xml query library in Elisp

;; Copyright (C) 2024 Pete Leng

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; The main purpose of this package is to retrieve
;; a list of destination nodes which are nested within
;; the source node, where the search begins,
;; such that the path from the source to each destination node
;; matches the given pattern.
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

(require 'dom)

;;; Code:
(defun node-tag (node)
  "Get tag of node NODE.
Wrapper function for dom-tag."
  (dom-tag node))

(defun node-children (node)
  "Get children of node NODE."
  (and (consp node) (cddr node)))

(defun node-attrs (node)
  "Get attribute list of node NODE."
  (and (consp node) (cadr node)))

(defun node-attr (attr node)
  "Get attribute ATTR of node NODE."
  (and (consp node)
       (cdr (assoc attr (nth 1 node)))))

(defun visit-node (func node)
  "Recursively walk the node NODE and apply function FUNC to each node."
  (funcall func node)
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
  "Match the tag of exml node N1 and N2."
  (equal (node-tag n1) (node-tag n2)))

(defun match-attr (patt node)
  "Match an attribute PATT against a exml node NODE.
An attribute looks like (op name val)."
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
	       (`(,_ . ,_) (error "Operator not implemented"))
	       )
	     t))))

(defun match-attrs (patts node)
  "Match a list of attributes PATTS against a node NODE."
  (or (null patts)
      (seq-every-p (lambda (patt)
		     (match-attr patt node))
		   patts)))

(defun match-patt (patt nd)
  "Match node ND with a pattern PATT.
Return the list of matched node or nodes (in case of wildcard pattern).
PATT is a node-like object in intermediate representation."
  (and (not (null patt)) ;; empty pattern does not make sense.
       (consp nd) ;; nd has to be a list, i.e., not a leaf node.
       (pcase patt
	 (`() ;; should not happen, check regardless
	  (error "Empty pattern"))
	 (`("*" . ,_) (match-wc patt nd))
	 (`(,tag ,attrs)
	  (and (equal tag (node-tag nd))
	       (match-attrs attrs nd)
	       (list nd)))
	 (`(,tag ,attrs . ,rest)
	  (and (equal tag (node-tag nd))
	       (match-attrs attrs nd)
	       (match-patts rest (node-children nd))))
	 (_ (error "Malformed pattern: %s" patt))))
  )

(defun match-wc (patt nd)
  "Match node ND wigh a wildcard pattern PATT.
Return a list of node or nodes matched."
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
  "Match a list of patterns PATTS against a list of nodes NODES.
Return a list of matching nodes."
  (and (not (null patts)) ;; empty patterns do not make sense
       nodes
       (pcase patts
	 (`() ;; not happening, check anyway
	  (error "Empty pattern list"))
	 (`(("*" . ,_) . ,_) (match-patts-wc patts nodes))
	 ;; (`(("~" . ,_) . ,rest)) ;; supported by "*"
	 (`(,patt) (match-patt patt (car nodes)))
	 (`(,patt . ,rest) (and (match-patt patt (car nodes))
				(match-patts rest (cdr nodes))))))
  )


(defun match-patts-wc (patts nodes)
  "Match a pattern list PATTS against a list of nodes NODES.
The first pattern in the list PATTS is a wildcard pattern.
Return a list of matching nodes."
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
;; Given the following node and following patterns,

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
;;; exml-query.el ends here
