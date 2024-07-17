;; -*- lexical-binding: t -*-
(require 'shr)

;; not necessary since call-process searches in exec-path
(defvar epub-unzip-command "unzip"
  "Commnad to unzip epub files.")

(defvar epub-progress-file (locate-user-emacs-file "epub.prog")
  "File used for saving and restoring reading progress.")

(defvar-local epub-unzip-exdir nil
  "Temporary directory to which the epub file extracts.")

(defvar-local epub-root-url nil
  "Base URL of the OCF container.")

(defvar-local epub-container-file-url nil
  "URL of the container.xml file.")

(defvar-local epub-package-doc-url nil
  "URL of the package document.")

(defvar-local epub-publication-id nil
  "Unique identifier for the EPUB publication.")

(defvar-local epub-manifest-table nil
  "A table from content id to url and media-type.")

(defvar-local epub-url-table nil
  "A table from content url to id.")

(defvar-local epub-spine-alist nil
  "A list that specifies reading order.")

(defvar-local epub-toc-id nil
  "File id of the TOC document.")

(defvar-local epub-current-content-id nil
  "File id of the document currently reading.")

(defvar-local epub-scroll-pct 0.75
  "File id of the document currently reading.")

;; Unzip epub file
(defun epub-unzip (fpath exdir)
  ;; note that fpath and exdir are strings and absolute
  ;; literal strings containing ~ will not undergo shell expansion properly
  (call-process epub-unzip-command nil "*epub unzip*" nil "-o" fpath "-d" exdir))

;; Cleanup function
(defun epub-cleanup (&optional err)
  ;; save progress unimplemented
  (unless err (epub-save-progress))
  (delete-directory epub-unzip-exdir t))

(defun epub-cleanup-all ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'epub-mode)
	(epub-cleanup)))))

(defun epub-parse-xml (fpath)
  (with-temp-buffer
    (insert-file-contents fpath)
    (libxml-parse-xml-region (point-min) (point-max)))
  )

(defun epub-url-remotep (url)
  "Return url if it is remote, nil otherwise."
  (and (url-type (url-generic-parse-url url)) url))

(defun epub-locate-container-file (dir)
  "Given the directory (that includes the EPUB OCF container) dir,
return absolute URL of the container.xml file,
typicall located under **/root/META-INF/."
  (named-let locate-container ((dir-list `(,dir)))
    (or (locate-file "container.xml" dir-list)
	(locate-container (seq-filter #'file-directory-p
				      (mapcan (lambda (f)
						(directory-files f t "[^.]"))
					      dir-list))))))

(defun epub-locate-package-doc (container-file-url)
  "Given the url of the container file 'container.xml',
return the absolute url of the package document.
The relative url is found in the 'rootfile' element, after parsing the container file."
  ;; Package document url is relative to the root URL,
  ;; see https://www.w3.org/TR/epub/#sec-parsing-urls-metainf
  (let* ((pt (epub-parse-xml container-file-url))
	 (node (exml-find '("*" nil (rootfile nil))
			  pt)))
    (expand-file-name (dom-attr node 'full-path) epub-root-url)))

;; The package document parse trees includes:
;; - the manifest element,
;; which provides an exhaustive list of publication resources for renderding.
;; - the spine element,
;; which defines an ordered list of manifest item references,
;; that represent the default reading order.
;; - the navigation document used as TOC,
;; not mandatorily included in the spine.
(defun epub-parse-package-doc ()
  "Parse the package document, located at 'epub-package-doc-url'."
  (let* ((pt (epub-parse-xml epub-package-doc-url))
	 (pid (epub-get-pub-id pt))
	 (nav-id (epub-get-nav-doc-id pt))
	 (max-lisp-eval-depth 12800) ;; for parsing large epub files
	 )
    (setq epub-publication-id pid)
    (setq epub-toc-id nav-id)
    (let ((manifest (epub-get-manifest-tb pt))
	  (spine (epub-get-spine-alist pt)))
      (message (format "pub id: %s" pid))
      (message (format "toc id: %s" nav-id))
      ;; (message (format "spine: %s" spine))
      (setq epub-manifest-table manifest)
      (setq epub-spine-alist spine)
      (let ((urltb (make-hash-table :test #'equal
				    :size (hash-table-count manifest))))
        (maphash (lambda (k val) (puthash (car val) k urltb))
		 manifest)
	(setq epub-url-table urltb)))))

(defun epub-get-pub-version (package-doc-pt)
  "Given the package document parse tree package-doc-pt,
return the publication version."
  (dom-attr package-doc-pt 'version))

(defun epub-get-pub-id (package-doc-pt)
  "Given the package document parse tree package-doc-pt,
return the publication unique identifier."
  (let ((node
	 (or (exml-find '("*" nil (identifier (("=" scheme "uuid"))))
			package-doc-pt)
	     (exml-find '("*" nil (identifier (("$=" id "ISBN"))))
			package-doc-pt))))
    (if node
	(car (node-children node))
      (let ((title
	     (exml-find '("*" nil (title nil)) package-doc-pt)))
	(md5 (car (node-children title)))))))

;; In EPUB 3.x, navigation document is declared using the "nav property",
;; within the manifest element,
;; see https://www.w3.org/TR/epub/#sec-item-resource-properties.
;; In EPUB 2.x, an NCX document is required for navigation,
;; item id specied by the toc attribute of the spine element,
;; see https://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm#Section2.4.
(defun epub-get-nav-doc-id (pt)
  "Given the package document parse tree pt,
return the item id of the navigation document, as indexed in the manifest."
  (pcase (epub-get-pub-version pt)
    ((and vs (guard (version< vs "3")))
     (let ((spine (exml-find '("*" nil (spine nil)) pt)))
       (dom-attr spine 'toc)))
    (_
     (let ((node (exml-find '("*" nil (item (("*=" properties "nav"))))
			    pt)))
       (dom-attr node 'id)))))

(defun epub-get-manifest-tb (package-doc-pt)
  "Given the package document parse tree package-doc-pt,
return a hashtable, mapped from (manifest) item id to absolute url and media type."
  (let* ((records (exml-findall '("*" nil (manifest nil ("*" nil) (item nil)))
				package-doc-pt))
	 (tb (make-hash-table :test #'equal
			      :size (length records))))
    ;; the url specified by the href attribute must be an absolute- or
    ;; path-relative-scheme-less-URL string,
    ;; see https://www.w3.org/TR/epub/#sec-item-elem.
    (mapc (lambda (node)
	    (let* ((id (dom-attr node 'id))
		   (href (dom-attr node 'href))
		   (type (dom-attr node 'media-type))
		   ;; base-url is pkg doc directory
		   (base-url (file-name-directory epub-package-doc-url))
		   (abs-href (or (epub-url-remotep href)
				 (expand-file-name href base-url))))
	      (puthash id (list abs-href type) tb)))
	  records)
    tb))

(defun epub-get-spine-alist (package-doc-pt)
  "Given the package document parse tree package-doc-pt,
return an association list representing the spine.
Each element in the array is a pair of item id and the id of the next item,
(id . id-next), with the last element being (last-id . nil)."
  (let* ((max-lisp-eval-depth 12800)
	 (nodes
	  (exml-findall '("*" nil (spine nil ("*" nil) (itemref nil)))
			package-doc-pt))
	 (ids
	  (mapcar (apply-partially #'node-attr 'idref) nodes))
	 (ids-shift1 (cdr (nconc ids '(nil)))) ;; note ids is changed due to side effect.
	 (spine (cl-mapcar #'cons ids ids-shift1))
	 ;; since toc is possibly absent in spine,
	 ;; add toc to front if not included already.
	 (spine (if (and epub-toc-id (assoc epub-toc-id spine))
		    spine
		  (cons (cons epub-toc-id (caar spine)) spine))))
    spine))

;; Custom rendering functions overriding default 'shr-tag-img'.
(defun epub-tag-img (dom)
  (let ((url (dom-attr dom 'src)))
    (if (epub-url-remotep url)
	(shr-tag-img dom)
      ;; resolve url locally if pointing to a resouce in the container.
      (let* ((start (point-marker))
	     (title (dom-attr dom 'title))
	     (alt (or (dom-attr dom 'alt)
		      "missing alt attr"))
	     (width (shr-string-number (dom-attr dom 'width)))
	     (height (shr-string-number (dom-attr dom 'height)))
	     (url (expand-file-name url)) ;; current buffer directory is expanded upon
	     ;; as urls in content documents are relative to the document itself.
	     (image (epub-create-image url (list :width width :height height))))
	(when image
	  (when (and (> (current-column) 0)
		     (> (car (image-size image t)) 400))
	    (insert "\n"))
	  (insert-image image alt)
	  ;; multi-frame image unimplemented...
	  (when (zerop shr-table-depth) ;; not in a table.
	    ;; (put-text-property start (point) 'keymap shr-image-map)
	    (put-text-property start (point) 'shr-alt alt)
	    (put-text-property start (point) 'image-url url)
	    (put-text-property start (point) 'help-echo
			       (shr-fill-text (or title alt)))))))))

(defun epub-create-image (url &optional flags)
  "Given a resource url, return an image object.
A wrapper for 'create-image'."
  ;; create image within window limit if window is assigned
  (if (null (get-buffer-window (current-buffer) t))
      (create-image url nil nil :ascent 100) ;; no existing window for buffer
    (seq-let (lft top rgt bot) (window-body-pixel-edges
				(get-buffer-window (current-buffer)))
      (let ((width (plist-get flags :width))
	    (height (plist-get flags :height))
	    (max-width (truncate (* shr-max-image-proportion (- rgt lft))))
	    (max-height (truncate (* shr-max-image-proportion (- bot top))))
	    (scaling (image-compute-scaling-factor image-scaling-factor)))
	;; (message (format "img %s width: %s, height %s" url width height))
	;; scaling is no smaller than 1
	(if (and width height
		 (< (* width scaling) max-width)
		 (< (* height scaling) max-height))
            (create-image url (shr--image-type) nil
			  :ascent 100
			  :width width
			  :height height)
          (create-image url (shr--image-type) nil
			:ascent 100
			:max-width max-width
			:max-height max-height))))))

;; Override shr-url-transformer in <a> tag such that,
;; the url passed into 'urlify' is absolute.
;; Bind keymap to epub-button-map (instead of shr-map) inside 'urlify'.
;; 'urlify' is also called in tags including <video> and <audio>,
;; which most definitely do not point to resources inside container.
(defun epub-tag-a (dom)
  ;; unless remote url, expand relative url to absolute url.
  (let ((shr-url-transformer #'(lambda (url)
				 (if (epub-url-remotep url) url
				   (expand-file-name url))))
	(shr-map epub-button-map))
    (shr-tag-a dom)))

;; Translate ncx to html (for backward compatibility).
;; NCX files for navigation are replaced by navigation document since 3.x.
;; https://www.w3.org/TR/epub/#sec-opf2-ncx
(defun epub-ncx-to-html (dom)
  (with-temp-buffer
    (epub--ncx-to-html dom)
    (buffer-string)))

(defun epub--ncx-to-html (dom)
  (pcase (node-tag dom)
    ('navMap
     (insert "<ol>\n")
     (mapc #'epub--ncx-to-html (node-children dom))
     (insert "</ol>\n"))
    
    ('navPoint
     (let* ((label (exml-find '("*" nil (text nil)) dom))
	    (content (exml-find '("*" nil (content nil)) dom))
	    (label (car (dom-children label)))
	    (href (expand-file-name (node-attr 'src content)
				    epub-root-url)))
       (unless href
	 (warn "content href missing: %s" label))
       (insert "<li>\n")
       (insert (format "<a href=\"%s\">%s</a>\n"
		       (xml-escape-string (or href " "))
		       (xml-escape-string (or label "missing label"))))
       (if-let ((children
		 (exml-findall '(navPoint nil ("*" nil) (navPoint nil))
			       dom)))
	   (mapc #'epub--ncx-to-html children))
       (insert "</li>\n")))
    (tag (error (format "cannot handle tag: %s" tag)))))

(defun epub--ncx-descend (dom)
  "Transform a ncx dom tree to an xml dom tree."
  (pcase (node-tag dom)
    ('navMap
     (let ((ch
	    (mapcar #'epub--ncx-descend (node-children dom))))
       `(ol nil ,@ch)))
    
    ('navPoint
     (let* ((label (exml-find '("*" nil (text nil)) dom))
	    (label (car (dom-children label)))
	    (content (exml-find '("*" nil (content nil)) dom))
	    (href (node-attr 'src content))
	    ;; (href (expand-file-name (node-attr 'src content) epub-root-url))
	    (ch (exml-findall '(navPoint nil ("*" nil) (navPoint nil)) dom))
	    (ch (and ch (mapcar #'epub--ncx-descend ch))))
       (unless href
	 (warn "content href missing: %s" label))
       `(li nil (a ((href . ,href)) ,label) ,@ch)))
    (tag (error (format "cannot handle tag: %s" tag)))))

;; keymap
(defvar-keymap epub-mode-map
  "n" 'epub-next-chap
  "p" 'epub-prev-chap
  "t" 'epub-goto-toc
  "SPC" 'epub-scroll-up
  "RET" 'epub-scroll-up
  "S-SPC" 'epub-scroll-down
  "DEL" 'epub-scroll-down
  )

(defvar-keymap epub-button-map
  :parent shr-map
  "<mouse-2>" 'epub-browse-url
  "RET" 'epub-browse-url)

;; Retrive content information using manifest id
(defun manifest-get (cid prop)
  "Given content id cid and property prop,
return property value from the manifest table."
  (let ((vals (gethash cid epub-manifest-table)))
    (unless vals
      (message (format "content id: %s" cid))
      (error (format "content id not exist: %s" cid))
      (throw 'exit-without-save t))
    (pcase prop
      (:url (nth 0 vals))
      (:media-type (nth 1 vals))
      (_ (error (format "unknown content property: %s" prop))))))

;; Browse url
(defun epub-browse-url (&optional mouse-event)
  (interactive (list last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((null url)
      (message "No link under point"))
     ((epub-url-remotep url)
      (browse-url url))
     (t
      (let ((url (url-generic-parse-url url)))
	(epub-goto-content-helper (url-filename url) nil (url-target url)))
      ))))

;; The lowest level fucntion for navigating content documents.
(defun epub-goto-content-helper (url-or-id &optional pnt target)
  (pcase (gethash url-or-id epub-manifest-table)
    (`(,url ,tp)
     (epub-goto-content url-or-id url pnt target))
    ((and 'nil
	  (let cid (gethash url-or-id epub-url-table)))
     (epub-goto-content cid url-or-id pnt target))
    (_ (error "neither url or id, should not happen"))))

(defun epub-goto-content (cid url pnt target)
  "Given content id an url, render content document."
  (let ((tp (manifest-get cid :media-type)))
    (message (format "goto url: %s" url))
    (epub-render-content url tp)
    ;; update current content id.
    (setq epub-current-content-id cid))
  
  (when pnt
    (message (format "goto pnt: %s" pnt))
    (goto-char pnt))
  
  (when target
    (message (format "goto target: %s" target))
    (text-property-search-forward 'shr-target-id
				  target
				  (lambda (target ids)
				    (member target ids)))))

(defun epub-render-content (url &optional tp)
  "Render the specified content in the current buffer."
  (let ((tp (or tp
		(manifest-get (gethash url epub-url-table)
			      :media-type))))
    (pcase tp
      ((pred (string-match "dtbncx")) ;; A ncx TOC file, requires translation
       (let* ((pt (epub-parse-xml url))
	      (dom (exml-find '("*" nil (navMap nil)) pt))
	      (dom (epub--ncx-descend dom)))
	 (epub-render-html dom epub-root-url)))
      (_
       (let ((dom (epub-parse-xml url)))
	 (epub-render-html dom url))))
    ))

(defun epub-render-html (dom &optional url)
  "Render the html content file in the current buffer.
Url is necessary to resolve dom elements with relative urls to absolute urls."
  (let ((shr-external-rendering-functions '((img . epub-tag-img)
					    (a . epub-tag-a)))
	(default-directory (if url (file-name-directory url)
			     default-directory))
	buffer-read-only ;; so that buffer can be modified
	)
    (erase-buffer)
    (shr-insert-document dom)
    (set-buffer-modified-p nil)
    (goto-char (point-min)) ;; start at the front by default
    ))

(defun epub-next-chap ()
  "Go to next content in the spine."
  (interactive)
  ;; save progress on current content if at neither front or end.
  (let ((id (cdr (assoc epub-current-content-id
			epub-spine-alist))))
    (when id
      (epub-goto-content-helper id))))

(defun epub-prev-chap ()
  "Go to previous content in the spine."
  (interactive)
  (let ((id (car (rassoc epub-current-content-id
			 epub-spine-alist))))
    (when id
      (epub-goto-content-helper id))))

(defun epub-goto-toc ()
  "Go to toc content."
  (interactive)
  (epub-goto-content-helper epub-toc-id))

(defun epub-scroll-up (arg)
  (interactive "P")
  (if (>= (window-end) (point-max))
      (epub-next-chap)
    (let ((wid-lines
	   (count-screen-lines (window-start) (window-end))))
      (scroll-up (or arg
		     (truncate (* epub-scroll-pct wid-lines)))))))

(defun epub-scroll-down (arg)
  (interactive "P")
  (if (<= (window-start) (point-min))
      (progn
	(epub-prev-chap)
	(goto-char (point-max)))
    (let ((wid-lines
	   (count-screen-lines (window-start) (window-end))))
      (scroll-down (or arg
		     (truncate (* epub-scroll-pct wid-lines)))))))

(defun epub-dquote (str)
  (concat "\"" str "\""))

(defun epub-dquote-pents (ents)
  (mapcar (lambda (ent)
	    (seq-let (pid cid pnt) ent
	      (list (epub-dquote pid)
		    (epub-dquote cid)
		    pnt)))
	  ents))

(defun epub-save-progress ()
  "Save current reading progress to designated file, 'epub-progress-file'.
Progress data is a list of progress entries, each is a list of (publication-id, content-id, point)."
  (when epub-progress-file
    (let* ((prev-ents (epub-retrive-progress-all))
	   (ent
	    (list epub-publication-id epub-current-content-id (point)))
	   (new-ents
	    (cons ent (assoc-delete-all epub-publication-id prev-ents))))
      ;; (message (format "save progress: %s" new-ents))
      (with-temp-file epub-progress-file ;; progress file is overwritten
	(let ((ents (epub-dquote-pents new-ents)))
	  ;; format strips one layer of quote from string elements
	  ;; while converting list, thus the need to double quote
	  (insert (format "%s" ents)))))))

(defun epub-retrive-progress-all ()
  "Return all progress data (a list), or nil if empty."
  (when (and epub-progress-file
	     (file-exists-p epub-progress-file))
    (with-temp-buffer
      (insert-file-contents epub-progress-file)
      (condition-case err
	  (read (buffer-string))
	(error (format "Failed to retrieve progress: %s" err))))))

(defun epub-retrive-progress (id)
  "Return progress entry for specific publication if given id, return nil if the corresponding publication id is not found."
  ;; read is used when reading from progress file, changing integer strings to integers.
  (assoc id (epub-retrive-progress-all)))

;; major mode
(define-derived-mode epub-mode special-mode "EPUB"
  "Major mode for epub files"
  (add-hook 'change-major-mode-hook 'epub-cleanup nil t)
  (add-hook 'kill-buffer-hook 'epub-cleanup nil t)
  (add-hook 'kill-emacs-hook 'epub-cleanup-all)
  (when (null buffer-file-name)
    (error "EPUB file not specified"))

  ;; Extract EPUB file to temporary directory
  (setq epub-unzip-exdir (make-temp-file "epub-" t))
  (pcase (epub-unzip buffer-file-name epub-unzip-exdir)
    ((and status
	  (guard (null (integerp status))))
     (epub-cleanup)
     (error "EPUB extraction failed: %s" status))
    ((and status
	  (guard (> status 1)))
     (epub-cleanup)
     (error "EPUB extraction exited: %s" status))
    (stat (message (format "extraction success: %s" stat))))
  
  ;; Locate container file
  (setq epub-container-file-url
	(epub-locate-container-file epub-unzip-exdir))
  (unless epub-container-file-url
    (error "container.xml not found."))
  
  ;; Locate container root directory
  ;; Root url is the grandparent of 'epub-container-file-url',
  ;; as in root/META-INF/container.xml.
  (setq epub-root-url (file-name-directory
		       (directory-file-name
			(file-name-directory epub-container-file-url))))

  ;; Parse container file
  (setq epub-package-doc-url (epub-locate-package-doc epub-container-file-url))
  (unless (file-exists-p epub-package-doc-url)
    (error "EPUB package document not found."))

  ;; Parse the package document
  (epub-parse-package-doc)

  ;; Misc
  (setq buffer-undo-list t)

  (if-let ((prog (epub-retrive-progress epub-publication-id))
	   (cid (nth 1 prog))
	   (pnt (nth 2 prog)))
      (progn
	(message (format "previous progress: %s" prog))
	(condition-case err
	    (epub-goto-content-helper cid pnt)
	  (epub-cleanup t)))
    (let ((front (cdar epub-spine-alist)))
      (epub-goto-content-helper front))))

;; epub.el ends here.
