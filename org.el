;; ============================================================================
;;                             Org mode
;; ============================================================================
(require 'org-tempo)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (sql . t)))

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
  (add-hook 'org-mode-hook #'visual-line-mode))

;; Enable asynchronous execution of src blocks
(when (package-installed-p 'ob-async)
  (require 'ob-async)
  (add-hook 'ob-async-pre-execute-src-block-hook
            '(lambda ()
	       (require 'ob-sql-mode)
	       (require 'hive2))))

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
