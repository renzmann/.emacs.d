;; ============================================================================
;;                             Org mode
;; ============================================================================
;; Beefy startup hit from requiring this eagerly, and not sure if I even use
;; any of its features...
;; (require 'org-tempo)

(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)

;; A kill-block command for working with src blocks
(defun renz/org-kill-src-block ()
  "Kill the src block around point, if applicable."
  (interactive)
  (org-babel-remove-result)
  (org-mark-element)
  (kill-region nil nil t))

;; https://willschenk.com/articles/2019/using_org_mode_in_hugo/
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (shell . t)))

  ;; Enable asynchronous execution of src blocks
  (when (package-installed-p 'ob-async)
    (require 'ob-async)
    (add-hook 'ob-async-pre-execute-src-block-hook
              #'(lambda ()
	          (require 'ob-sql-mode)
	          (require 'hive2))))
  )

(setq ob-async-no-async-languages-alist '("python"))
(setq org-html-htmlize-output-type 'css)

;; For navigating to tangled src blocks
;; https://emacs.stackexchange.com/a/69591
(defun renz/org-babel-tangle-jump-to-src ()
  "The opposite of `org-babel-tangle-jump-to-org'. Jumps at tangled code from org src block."
  (interactive)
  (if (org-in-src-block-p)
      (let* ((header (car (org-babel-tangle-single-block 1 'only-this-block)))
             (tangle (car header))
             (lang (caadr header))
             (buffer (nth 2 (cadr header)))
             (org-id (nth 3 (cadr header)))
             (source-name (nth 4 (cadr header)))
             (search-comment (org-fill-template
                              org-babel-tangle-comment-format-beg
                              `(("link" . ,org-id) ("source-name" . ,source-name))))
             (file (expand-file-name
                    (org-babel-effective-tangled-filename buffer lang tangle))))
        (if (not (file-exists-p file))
            (message "File does not exist. 'org-babel-tangle' first to create file.")
          (find-file file)
          (beginning-of-buffer)
          (search-forward search-comment)))
    (message "Cannot jump to tangled file because point is not at org src block.")))

;; TODO states
(setq org-todo-keywords '((sequence "TODO" "DEAD" "DONE")))


;; ============================================================================
;;                             Org-roam
;; ============================================================================
;; (setq org-roam-directory (file-truename "~/.emacs.d/org/org-roam")

;; Consider the below for Windows only
;; (setq org-roam-database-connector 'sqlite3)

;; (org-roam-db-autosync-mode)

;; ============================================================================
;; Displaying inline images from a remote source
;; ============================================================================
;; http://kychoi.org/blog/2014/11/02/Org-Display-Inline-Remote-Images
;; (defun org-display-inline-images (&optional include-linked refresh beg end)
;;   "Display inline images.

;; An inline image is a link which follows either of these
;; conventions:

;;   1. Its path is a file with an extension matching return value
;;      from `image-file-name-regexp' and it has no contents.

;;   2. Its description consists in a single link of the previous
;;      type.

;; When optional argument INCLUDE-LINKED is non-nil, also links with
;; a text description part will be inlined.  This can be nice for
;; a quick look at those images, but it does not reflect what
;; exported files will look like.

;; When optional argument REFRESH is non-nil, refresh existing
;; images between BEG and END.  This will create new image displays
;; only if necessary.  BEG and END default to the buffer
;; boundaries."
;;   (interactive "P")
;;   (when (display-graphic-p)
;;     (unless refresh
;;       (org-remove-inline-images)
;;       (when (fboundp 'clear-image-cache) (clear-image-cache)))
;;     (org-with-wide-buffer
;;      (goto-char (or beg (point-min)))
;;      (let ((case-fold-search t)
;; 	   (file-extension-re (org-image-file-name-regexp)))
;;        (while (re-search-forward "[][]\\[\\(?:file\\|[./~]\\)" end t)
;; 	 (let ((link (save-match-data (org-element-context))))
;; 	   ;; Check if we're at an inline image.
;; 	   (when (and (equal (org-element-property :type link) "file")
;; 		      (or include-linked
;; 			  (not (org-element-property :contents-begin link)))
;; 		      (let ((parent (org-element-property :parent link)))
;; 			(or (not (eq (org-element-type parent) 'link))
;; 			    (not (cdr (org-element-contents parent)))))
;; 		      (org-string-match-p file-extension-re
;; 					  (org-element-property :path link)))
;; 	     (let ((file (substitute-in-file-name (expand-file-name (org-element-property :path link)))))
;; 	       (when (file-exists-p file)
;; 		 (let ((width
;; 			;; Apply `org-image-actual-width' specifications.
;; 			(cond
;; 			 ((not (image-type-available-p 'imagemagick)) nil)
;; 			 ((eq org-image-actual-width t) nil)
;; 			 ((listp org-image-actual-width)
;; 			  (or
;; 			   ;; First try to find a width among
;; 			   ;; attributes associated to the paragraph
;; 			   ;; containing link.
;; 			   (let ((paragraph
;; 				  (let ((e link))
;; 				    (while (and (setq e (org-element-property
;; 							 :parent e))
;; 						(not (eq (org-element-type e)
;; 							 'paragraph))))
;; 				    e)))
;; 			     (when paragraph
;; 			       (save-excursion
;; 				 (goto-char (org-element-property :begin paragraph))
;; 				   (when
;; 				       (re-search-forward
;; 					"^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
;; 					(org-element-property
;; 					 :post-affiliated paragraph)
;; 					t)
;; 				     (string-to-number (match-string 1))))))
;; 			   ;; Otherwise, fall-back to provided number.
;; 			   (car org-image-actual-width)))
;; 			 ((numberp org-image-actual-width)
;; 			  org-image-actual-width)))
;; 		       (old (get-char-property-and-overlay
;; 			     (org-element-property :begin link)
;; 			     'org-image-overlay)))
;; 		   (if (and (car-safe old) refresh)
;; 		       (image-refresh (overlay-get (cdr old) 'display))
;; 		     (let ((image
;; 			    (create-image (if (org-file-remote-p file)
;; 					      (let* ((tramp-tmpdir (concat
;; 								    (if (featurep 'xemacs)
;; 									(temp-directory)
;; 								      temporary-file-directory)
;; 								    "/tramp"
;; 								    (file-name-directory (expand-file-name file))))
;; 						     (newname (concat
;; 							       tramp-tmpdir
;; 							       (file-name-nondirectory (expand-file-name file)))))
;; 						(make-directory tramp-tmpdir t)
;; 						(if (file-newer-than-file-p file newname)
;; 						    (copy-file file newname t t))
;; 						newname)
;; 					    file)
;; 					  (and width 'imagemagick)
;; 					  nil
;; 					  :width width)))
;; 		       (when image
;; 			 (let* ((link
;; 				 ;; If inline image is the description
;; 				 ;; of another link, be sure to
;; 				 ;; consider the latter as the one to
;; 				 ;; apply the overlay on.
;; 				 (let ((parent
;; 					(org-element-property :parent link)))
;; 				   (if (eq (org-element-type parent) 'link)
;; 				       parent
;; 				     link)))
;; 				(ov (make-overlay
;; 				     (org-element-property :begin link)
;; 				     (progn
;; 				       (goto-char
;; 					(org-element-property :end link))
;; 				       (skip-chars-backward " \t")
;; 				       (point)))))
;; 			   (overlay-put ov 'display image)
;; 			   (overlay-put ov 'face 'default)
;; 			   (overlay-put ov 'org-image-overlay t)
;; 			   (overlay-put
;; 			    ov 'modification-hooks
;; 			    (list 'org-display-inline-remove-overlay))
;; 			   (push ov org-inline-image-overlays)))))))))))))))
