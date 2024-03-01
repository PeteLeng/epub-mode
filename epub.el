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

(defvar-local epub-container-url nil
  "URL of the container.xml file.")

(defvar-local epub-package-url nil
  "URL of the package document.")

(defvar-local epub-publication-id nil
  "Unique identifier for the EPUB publication.")

(defvar-local epub-manifest-table nil
  "A table from content file ids to urls.")

(defvar-local epub-id-table nil
  "A table used for searching manifest id using absolute url.")

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
  "Return url if it is remote, nil otherwise"
  (and (url-type (url-generic-parse-url url)) url))

(defun epub-locate-container-file (dir)
  "Return absolute URL of the container.xml file, under root/META-INF/"
  (named-let locate-container ((dir-list `(,dir)))
    (or (locate-file "container.xml" dir-list)
	(locate-container (seq-filter #'file-directory-p
				      (mapcan (lambda (f)
						(directory-files f t "[^.]"))
					      dir-list))))))

(defun epub-locate-package-doc (container-doc-path base-url)
  "Parse the 'container.xml' file for the absolute url (fpath) of the package document,
return URL."
  ;; URLs in the container file are relative to the root URL
  ;; https://www.w3.org/TR/epub/#sec-parsing-urls-metainf
  (let* ((pt (epub-parse-xml container-doc-path))
	 (node (exml-find '("*" nil (rootfile nil))
			  pt)))
    (expand-file-name (dom-attr node 'full-path) base-url)))

;; The package document contains:
;; - the manifest element,
;; which provides an exhaustive list of publication resources for renderding.
;; - the spine element,
;; which defines an ordered list of manifest item references
;; that represent the default reading order.
;; - information on the required navigation document,
;; though it is not mandatory to include it in the spine.
(defun epub-parse-package-doc ()
  (let* ((pt (epub-parse-xml epub-package-url))
	 (pid (epub-package-identifier pt))
	 (nav-id (epub-package-navigation-doc pt)))
    (setq epub-publication-id pid)
    (setq epub-toc-id nav-id)
    (let ((manifest (epub-package-manifest pt epub-package-url))
	  (spine (epub-package-spine pt epub-package-url)))
      (message (format "pid: %s" pid))
      (message (format "tid: %s" nav-id))
      (message (format "spine: %s" spine))
      (setq epub-manifest-table manifest)
      (setq epub-spine-alist spine)
      (let ((idtable (make-hash-table :test #'equal
				      :size (hash-table-count manifest))))
        (maphash (lambda (k val) (puthash (car val) k idtable))
		 manifest)
	(setq epub-id-table idtable)))))

(defun epub-package-version (package-doc-pt)
  (dom-attr package-doc-pt 'version))

(defun epub-package-identifier (package-doc-pt)
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

;; In EPUB 3.x, navigation document is specified in the manifest,
;; using the 'nav proterty'.
;; https://www.w3.org/TR/epub/#sec-item-resource-properties
;; In EPUB 2.x, ncx document serves as the navigation document (toc).
;; https://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm#Section2.4
(defun epub-package-navigation-doc (pt)
  "Return the manifest id of the navigation document."
  (let* ((vs (epub-package-version pt))
	 (node (cond
		((version< vs "3")
		 (let* ((spine (exml-find '("*" nil (spine nil)) pt))
			(fid (dom-attr spine 'toc)))
		   (exml-find `("*" nil (item (("=" id ,fid)))) pt)))
		(t
		 (exml-find '("*" nil (item (("*=" properties "nav")))) pt)))))
    (dom-attr node 'id)))

(defun epub-package-manifest (package-doc-pt base-url)
  "Search the package document parse tree, and return a hashtable of container files,
mapped from file ids to absolute urls."
  (setq base-url (file-name-directory base-url))
  (let* ((records (exml-findall '("*" nil (manifest nil ("*" nil) (item nil)))
				package-doc-pt))
	 (tb (make-hash-table :test #'equal
			      :size (length records))))
    (mapc (lambda (node)
	    (let ((id (dom-attr node 'id))
		  (href (dom-attr node 'href))
		  (type (dom-attr node 'media-type)))
	      (let ((abs-url (or (epub-url-remotep href)
				 (expand-file-name href base-url))))
		(puthash id (list abs-url type) tb))))
	  records)
    tb))

(defun epub-package-spine (package-doc-pt base-url)
  "Search the package document parse tree,
and return the spine object."
  (let* ((nodes
	  (exml-findall '("*" nil (spine nil ("*" nil) (itemref nil)))
			package-doc-pt))
	 (ids
	  (mapcar (apply-partially #'node-attr 'idref) nodes))
	 (ids-shift (nconc (cdr ids) '(nil)))
	 (spine (cl-mapcar #'cons ids ids-shift))
	 ;; include toc if not already
	 ;; toc is not mandatory to be included in the spine element
	 (spine (if (and epub-toc-id (assoc epub-toc-id spine))
		    spine
		  (cons (cons epub-toc-id (caar spine)) spine))))
    spine))

;; Custom rendering functions used by shr to override default.
;; Urls in the content files are relative to the "content url"
;; of the files in which they appear.
(defun epub-tag-img (dom)
  (let ((url (dom-attr dom 'src)))
    (if (epub-url-remotep url)
	(shr-tag-img dom)
      (let* ((start (point-marker))
	     (title (dom-attr dom 'title))
	     (alt (or (dom-attr dom 'alt)
		      "*"))
	     (width (shr-string-number (dom-attr dom 'width)))
	     (height (shr-string-number (dom-attr dom 'height)))
	     (url (expand-file-name url)) ;; use "content url" as base-url
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
  "A wrapper of 'create-image' for image urls within the EPUB container."
  ;; create image within window limit if window is assigned
  (if (null (get-buffer-window (current-buffer) t))
      (create-image url nil nil :ascent 100)
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
;; NCX files provide toc for epub 2.x,
;; which is replaced by navigation document since 3.x.
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
  "n" 'epub-next-content
  "p" 'epub-prev-content
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
(defun manifest-get (mid prop)
  (let ((vals (gethash mid epub-manifest-table)))
    (unless vals
      (error (format "content id not exist: %s" mid)))
    (pcase prop
      (:url (nth 0 vals))
      (:media-type (nth 1 vals))
      (_ (error (format "unknown manifest property: %s" prop))))))

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
	(epub-goto-content (url-filename url) nil (url-target url)))
      ))))

;; The lowest level fucntion for navigating container contents.
(defun epub-goto-content (url-or-id &optional id-p target)
  ;; browse content files (inside the container)
  ;; content can be identified by either absolute url,
  ;; or retrieved using its manifest id.
  (let* ((uid (if id-p url-or-id
		(gethash url-or-id epub-id-table)))
	 (url (if (null id-p) url-or-id
		(manifest-get url-or-id :url)))
	 (tp (manifest-get uid :media-type)))
    (message (format "go to: %s" url))
    (epub-render-content url tp)
    ;; update current content id.
    (setq epub-current-content-id uid))
  
  (when target
    (text-property-search-forward 'shr-target-id
				  target
				  (lambda (target ids)
				    (member target ids)))))

(defun epub-render-content (url &optional tp)
  "Render the specified content in the current buffer."
  (let ((tp (or tp
		(manifest-get (gethash url epub-id-table)
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

(defun epub-next-content ()
  "Go to next content in the spine."
  (interactive)
  ;; save progress on current content if at neither front or end.
  (let ((id (cdr (assoc epub-current-content-id
			epub-spine-alist))))
    (when id
      (epub-goto-content id t))))

(defun epub-prev-content ()
  "Go to previous content in the spine."
  (interactive)
  (let ((id (car (rassoc epub-current-content-id
			 epub-spine-alist))))
    (when id
      (epub-goto-content id t))))

(defun epub-goto-toc ()
  "Go to toc content."
  (interactive)
  (epub-goto-content epub-toc-id t))

(defun epub-scroll-up (arg)
  (interactive "P")
  (if (>= (window-end) (point-max))
      (epub-next-content)
    (let ((wid-lines
	   (count-screen-lines (window-start) (window-end))))
      (scroll-up (or arg
		     (truncate (* epub-scroll-pct wid-lines)))))))

(defun epub-scroll-down (arg)
  (interactive "P")
  (if (<= (window-start) (point-min))
      (progn
	(epub-prev-content)
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
	    ;; type massage due to using read in 'epub-retrive-progress-all'
	    (cons ent (assoc-delete-all epub-publication-id prev-ents))))
      ;; (message (format "save progress: %s" new-ents))
      (with-temp-file epub-progress-file ;; progress file is overwritten
	(let ((ents (epub-dquote-pents new-ents)))
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
  (add-hook 'kill-buffer-hook 'epub-cleanup)
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
     (error "EPUB extraction exited: %s" status)))
  
  ;; Locate container file
  (setq epub-container-url
	(epub-locate-container-file epub-unzip-exdir))
  (unless epub-container-url
    (error "container.xml not found."))
  
  ;; Set container root url
  ;; root url is the parent parent directory of the container file url
  ;; root/META-INF/container.xml
  (setq epub-root-url (file-name-directory
		       (directory-file-name
			(file-name-directory epub-container-url))))

  ;; Locate package document
  (setq epub-package-url
	(epub-locate-package-doc epub-container-url epub-root-url))
  (unless (file-exists-p epub-package-url)
    (error "EPUB package document not found."))

  ;; Parse the package document for information
  ;; version, identifier, nav (toc) doc, manifest, spine.
  (epub-parse-package-doc)

  ;; Misc
  (setq buffer-undo-list t)

  (if-let ((prog (epub-retrive-progress epub-publication-id))
	   (cid (nth 1 prog))
	   (pnt (nth 2 prog)))
      (progn
	(message (format "previous progress: %s" prog))
	(message (format "%s string?: %s" cid (stringp cid)))
	(unwind-protect
	    (epub-goto-content cid t pnt)
	  (epub-cleanup t)))
    (let ((front (cdar epub-spine-alist)))
      (epub-goto-content front t))))

;; epub.el ends here.
