;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: internal
;; URL: https://robbmann.io/

;;; Commentary:
;; A mostly minimal, reproducible Emacs configuration

;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun renz/package-sync ()
  "Remove unused sources and install any missing ones"
  (interactive)
  (package-autoremove)
  (package-install-selected-packages))

(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))

(when (memq system-type '(windows-nt cygwin ms-dos))
  ;; Set a better font on Windows
  (set-face-attribute 'default nil :font "Hack NF-12")

  ;; Alternate ispell when we've got msys on Windows
  (setq ispell-program-name "aspell.exe")

  ;; Set default shell to pwsh
  (setq explicit-shell-file-name "pwsh")
  )

(when (eq system-type 'darwin)
  ;; Uncomment this if we can't install Hack Nerd font
  ;; (set-face-attribute 'default nil :font "Menlo-14")
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-13")
  (exec-path-from-shell-initialize))

(use-package ef-themes
  :demand t
  :bind ("C-c m" . ef-themes-toggle)

  :init
  (setq ef-themes-headings
        '((0 . (1.9))
          (1 . (1.8))
          (2 . (1.7))
          (3 . (1.6))
          (4 . (1.5))
          (5 . (1.4)) ; absence of weight means `bold'
          (6 . (1.3))
          (7 . (1.2))
          (t . (1.1))))
  (setq ef-themes-to-toggle '(ef-cherie ef-summer))

  :config
  (load-theme 'ef-cherie :no-confirm))

(setq column-number-mode t
      mode-line-in-non-selected-windows t)

(setq eldoc-echo-area-use-multiline-p nil)

(setq history-length 25)
(savehist-mode 1)

(add-hook 'eshell-preoutput-filter-functions  'ansi-color-apply)

(recentf-mode t)

(setq-default fill-column 80)

(scroll-bar-mode -1)

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(set-face-background 'fringe (face-attribute 'default :background))

(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)

(setq-default indent-tabs-mode nil)

(defun renz/display-ansi-colors ()
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq mouse-wheel-tilt-scroll t)

(unless (version< emacs-version "27.1")
  (setq switch-to-buffer-obey-display-actions t))

(global-auto-revert-mode)

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)

;; Stop stupid bell
(setq ring-bell-function 'ignore)

(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (toggle-truncate-lines nil))

(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))
(add-hook 'yaml-mode-hook (lambda () (setq display-line-numbers 'relative)))
(unless (display-graphic-p)
  (add-hook 'text-mode-hook (lambda () (setq display-line-numbers 'relative))))

(delete-selection-mode t)

(xterm-mouse-mode 1)

(setq compilation-scroll-output t)

(defun renz/colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'renz/colorize-compilation-buffer)

(tool-bar-mode -1)

(advice-add 'risky-local-variable-p :override #'ignore)

(when (executable-find "rg")
  (setq grep-program "rg"))

(when (executable-find "fd")
  (setq find-program "fd"))

(setq confirm-kill-emacs 'yes-or-no-p)

(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(when (executable-find "aspell")
  (setq ispell-program-name "aspell"))

(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups/"))
      ;; auto-save-file-name-transforms
      ;; '(("." ,temporary-file-directory t))
      )

(put 'narrow-to-region 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun renz/--jump-section (dirname prompt extension)
  "For internal use: prompt for a file under `dirname' in the user
emacs config site with matching `extension' regexp"
  (find-file
   (concat dirname
	   (completing-read prompt
			    (directory-files dirname nil extension)))))

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)

(global-set-key (kbd "C-z") #'zap-up-to-char)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(global-set-key [remap list-buffers] 'ibuffer)

(use-package flymake
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(define-key isearch-mode-map (kbd "<C-return>")
  (defun isearch-done-opposite (&optional nopush edit)
    "End current search in the opposite side of the match."
    (interactive)
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end (goto-char isearch-other-end))))

(global-set-key (kbd "C-x C-p") 'previous-buffer)  ; Overrides `mark-page'
(global-set-key (kbd "C-x C-n") 'next-buffer)      ; Overrides `set-goal-column'

(global-set-key (kbd "C-c b") #'compile)
(global-set-key (kbd "C-c B") #'recompile)

(defun renz/find-tag ()
  "Use completing-read to navigate to a tag"
  (interactive)
  (xref-find-definitions (completing-read "Find tag: " tags-completion-table)))

(global-set-key (kbd "C-c d") #'renz/find-tag)

(global-set-key (kbd "C-c f") #'hippie-expand)

(setq renz/site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))

(defun renz/jump-configuration ()
  "Prompt for a .el file in my site-lisp folder, then go there."
  (interactive)
  (renz/--jump-section renz/site-lisp-dir
		       "Elisp config files: "
		       ".*\.el$"))

(defun renz/jump-init ()
  (interactive)
  (find-file (expand-file-name "README.org" user-emacs-directory))
  (consult-org-heading))

(global-set-key (kbd "C-c i i") #'renz/jump-init)
(global-set-key (kbd "C-c i l") #'renz/jump-configuration)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c j") #'toggle-window-split)

(global-set-key (kbd "C-c k") #'just-one-space)

(global-set-key (kbd "C-c q") #'replace-regexp)

(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'term)

(global-set-key (kbd "C-c w") #'whitespace-mode)

(global-set-key (kbd "C-c ;") #'comment-line)  ; TTY-friendly
(global-set-key (kbd "C-c <DEL>") #'backward-kill-sexp)  ;; TTY-frindly
(global-set-key (kbd "C-c <SPC>") #'mark-sexp)  ;; TTY-friendly

(use-package consult
  :bind(
        ;; C-x bindings (ctl-x-map)
        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

        ;; Other custom bindings
        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
        ("<help> a" . consult-apropos)            ;; orig. apropos-command
        ("C-c r" . consult-recent-file)

        ;; M-g bindings (goto-map)
        ("M-g e" . consult-compile-error)
        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
        ("M-g g" . consult-goto-line)             ;; orig. goto-line
        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
        ("M-g m" . consult-mark)
        ("M-g k" . consult-global-mark)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-imenu-multi)

        ;; M-s bindings (search-map)
        ("M-s d" . consult-find)
        ("M-s D" . consult-locate)
        ("M-s g" . consult-grep)
        ("M-s G" . consult-git-grep)
        ("M-s r" . consult-ripgrep)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi)
        ("M-s m" . consult-multi-occur)
        ("M-s k" . consult-keep-lines)
        ("M-s u" . consult-focus-lines)

        ;; Isearch integration
        ("M-s e" . consult-isearch-history)
        :map isearch-mode-map
        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

        ;; Minibuffer history
        :map minibuffer-local-map
        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
        ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  )

(setq completion-styles '(flex basic partial-completion emacs22))

(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless)

  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))

(setq completions-format 'one-column)

(setq completions-max-height 15)

(setq completion-auto-help 'visible
      completion-auto-select 'second-tab
      completion-show-help nil
      completions-sort nil
      completions-header-format nil)

(defun renz/sort-by-alpha-length (elems)
  "Sort ELEMS first alphabetically, then by length."
  (sort elems (lambda (c1 c2)
                (or (string-version-lessp c1 c2)
                    (< (length c1) (length c2))))))

(defun renz/sort-by-history (elems)
  "Sort ELEMS by minibuffer history.
Use `mct-sort-sort-by-alpha-length' if no history is available."
  (if-let ((hist (and (not (eq minibuffer-history-variable t))
                      (symbol-value minibuffer-history-variable))))
      (minibuffer--sort-by-position hist elems)
    (renz/sort-by-alpha-length elems)))

(defun renz/completion-category ()
  "Return completion category."
  (when-let ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       'category))))

(defun renz/sort-multi-category (elems)
  "Sort ELEMS per completion category."
  (pcase (renz/completion-category)
    ('nil elems) ; no sorting
    ('kill-ring elems)
    ('project-file (renz/sort-by-alpha-length elems))
    (_ (renz/sort-by-history elems))))

(setq completions-sort #'renz/sort-multi-category)

(define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

(setq tab-always-indent 'complete)

(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

(use-package tramp
  :defer t
  :config
  (setq vc-handled-backends '(Git)
        file-name-inhibit-locks t
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq tramp-use-ssh-controlmaster-options nil)

(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
  (add-to-list 'treesit-extra-load-path "~/.local/lib/"))

(setq renz/org-home "~/org/")
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)

(setq org-image-actual-width nil)

(defun renz/org-babel-tangle-jump-to-src ()
  "The opposite of `org-babel-tangle-jump-to-org'.
Jumps at tangled code from org src block."
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

(use-package org
  :hook
  ((org-mode . (lambda () (progn
                            (add-hook 'after-save-hook #'org-babel-tangle :append :local)
                            (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors)))))

  :init
  (defun renz/jump-org ()
    "Prompt for an org file in my emacs directory, then go there."
    (interactive)
    (renz/--jump-section renz/org-home "Org files: " ".*\.org$"))

  :bind
  (("C-c o a" . org-agenda)
   ("C-c o b d" . org-babel-detangle)
   ("C-c o b o" . org-babel-tangle-jump-to-org)
   ("C-c o b s" . renz/org-babel-tangle-jump-to-src)
   ("C-c o j" . consult-org-heading)
   ("C-c o k" . org-babel-remove-result)
   ("C-c o o" . renz/jump-org)
   ("C-c o w" . renz/org-kill-src-block)
   ("C-c o y" . ox-clip-image-to-clipboard))

  :config
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (shell . t)
     (fortran . t)
     (julia . t)
     ;; (jupyter . t)
     (scheme . t)
     (haskell . t)
     (lisp . t)
     (clojure . t)
     (C . t)
     (org . t)
     (gnuplot . t)
     (awk . t)
     (latex . t)))

  (setq org-agenda-files '("~/.emacs.d/org/work.org")
        org-hugo-front-matter-format "yaml"))

(use-package ob-async
  :after org
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            #'(lambda ()
                (require 'ob-sql-mode)
                (require 'hive2)))
  ;; Python has its own =:async yes= header argument we can use, so there's no
  ;; need to include it with ~ob-async~.
  (setq ob-async-no-async-languages-alist '("python"))
  ;; I'm having trouble remembering why I added this following line, except that I
  ;; believe it has something to do with exporting to HTML with syntax
  ;; highlighting.
  (setq org-html-htmlize-output-type 'css))

(use-package org-modern
  :after org
  :config
  (setq
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (if (display-graphic-p)
      (setq org-modern-table t)
    (setq org-modern-table nil))

  (global-org-modern-mode))

(use-package htmlize
  :after (org))

(use-package ox-clip
  :after org)

(use-package ox-hugo
  :after org)

(defun renz/sql-mode-hook ()
  (setq tab-width 4)
  (setq sqlformat-command 'sql-formatter))

(defvar renz/sql-indentation-offsets-alist
  '((syntax-error sqlind-report-sytax-error)
    (in-string sqlind-report-runaway-string)
    (comment-continuation sqlind-indent-comment-continuation)
    (comment-start sqlind-indent-comment-start)
    (toplevel 0)
    (in-block +)
    (in-begin-block +)
    (block-start 0)
    (block-end 0)
    (declare-statement +)
    (package ++)
    (package-body 0)
    (create-statement +)
    (defun-start +)
    (labeled-statement-start 0)
    (statement-continuation +)
    (nested-statement-open sqlind-use-anchor-indentation +)
    (nested-statement-continuation sqlind-use-previous-line-indentation)
    (nested-statement-close sqlind-use-anchor-indentation)
    (with-clause sqlind-use-anchor-indentation)
    (with-clause-cte +)
    (with-clause-cte-cont ++)
    (case-clause 0)
    (case-clause-item sqlind-use-anchor-indentation +)
    (case-clause-item-cont sqlind-right-justify-clause)
    (select-clause 0)
    (select-column sqlind-indent-select-column)
    (select-column-continuation sqlind-indent-select-column +)
    (select-join-condition ++)
    (select-table sqlind-indent-select-table)
    (select-table-continuation sqlind-indent-select-table +)
    (in-select-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (insert-clause 0)
    (in-insert-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (delete-clause 0)
    (in-delete-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (update-clause 0)
    (in-update-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)))

(defun renz/sql-indentation-offsets ()
  (setq sqlind-indentation-offsets-alist
        renz/sql-indentation-offsets-alist)
  (setq sqlind-basic-offset 4))

(use-package sql-indent
  :hook (sqlind-minor-mode . renz/sql-indentation-offsets))

(use-package sql-mode
  :hook ((sql-mode . renz/sql-mode-hook)
         (sql-mode . sqlup-mode)
         (sql-mode . sqlind-minor-mode)))

(use-package sqlup-mode
  :hook sql-interactive-mode)

(use-package hive2
  :after (sql)
  :mode ("\\.hql" . sql-mode))

(use-package ob-sql-mode
  :after (sql))

(use-package sqlformat
  :after (sql))

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))

(use-package python
  :mode ("\\.py" . python-ts-mode)
  :config
  (if (executable-find "mypy")
      (setq python-check-command "mypy"))
  (if (executable-find "pyright")
      (setq python-check-command "pyright"))
  (add-hook 'python-mode-hook #'blacken-mode))

(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)

(use-package pyrightconfig
  :after (python))

(use-package blacken
  :bind ("C-c p" . blacken-mode)
  :after (python))

(defun renz/md-hook ()
  "View buffer in visual fill mode with 80 character width."
  (interactive)
  (visual-fill-column-mode)
  (setq-local fill-column 80))

(use-package poly-markdown
  :after (markdown-mode)
  :mode ("\\.md" . poly-markdown-mode))

(use-package ahk-mode
  :mode "\\.ahk\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package git-gutter
  :demand t
  :bind ("C-c v s" . git-gutter:stage-hunk)
  :config (global-git-gutter-mode))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-alFh")
  (setq dired-dwim-target t))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-\\" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package coterm
  :config
  (coterm-mode))

(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package change-inner
  :bind (("C-c c i" . change-inner)
         ("C-c c o" . change-outer)
         ("C-c y i" . copy-inner)
         ("C-c y o" . copy-outer)))

(use-package eww
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html/?q="))

(use-package change-inner
  :bind (("C-c c i" . change-inner)
         ("C-c c o" . change-outer)
         ("C-c y i" . yank-inner)
         ("C-c y o" . yank-outer)))

(use-package esup
  :bind ("C-c x p")
  :config
  (setq esup-depth 0))

(use-package restart-emacs
  :bind ("C-c x r" . restart-emacs))

(use-package eglot
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)))

(server-start)

(provide 'init.el)
;;; init.el ends here
