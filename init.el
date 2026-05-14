;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: internal
;; URL: https://robbmann.io/

;;; Commentary:
;; A mostly minimal, reproducible Emacs configuration.

;;; Code:

;;;; Custom

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;;; Proxy

(defun renz/enable-proxy ()
  (interactive)
  "Turn on HTTP proxy."
  (let ((proxy-file (expand-file-name "proxy.el" user-emacs-directory)))
    (when (file-exists-p proxy-file)
      (load-file proxy-file)
      (setq url-proxy-services
            `(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . ,(concat renz/proxy-host ":" renz/proxy-port))
              ("https" . ,(concat renz/proxy-host ":" renz/proxy-port))))
      (setq url-http-proxy-basic-auth-storage
            (list
             (list
              (concat renz/proxy-host ":" renz/proxy-port)
              (cons renz/proxy-login
                    (base64-encode-string
                     (concat renz/proxy-login ":" (password-read "Proxy password: "))))))))))

(defun renz/disable-proxy ()
  (interactive)
  "Turn off HTTP proxy."
  (setq url-proxy-services nil)
  (setq url-http-proxy-basic-auth-storage nil))

;;;; Packages

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defun renz/windowsp ()
  "Are we on Microsoft Windows?"
  (memq system-type '(windows-nt cygwin ms-dos)))

;; MSYS gpg only understands /c/Users/... style paths, not C:/Users/...
(when-let* ((on-win (renz/windowsp))
            (has-uname (executable-find "uname"))
            (uname (shell-command-to-string "uname"))
            (is-msys (string-prefix-p "MSYS" uname))
            (package-dir-expandable (string-prefix-p "~" package-user-dir))
            (expand-package-dir (expand-file-name "gnupg" package-user-dir))
            (new-package-user-dir (replace-regexp-in-string "^\\([a-zA-Z]\\):/" "/\\1/" expand-package-dir)))
  (setq package-gnupghome-dir new-package-user-dir))

(defun renz/package-sync ()
  "Remove unused sources and install any missing ones."
  (interactive)
  (package-autoremove)
  (package-install-selected-packages)
  (package-vc-install-selected-packages))

(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))

;;;; OS -- Windows

(defun renz/find-file (chosen-dir regex)
  (interactive "DSearch dir: \nsRegexp: ")
  (let ((chosen-file (completing-read "File: " (find-lisp-find-files chosen-dir regex))))
    (find-file chosen-file)))

(global-set-key (kbd "C-c f f") #'renz/find)
(global-set-key (kbd "C-c f d") #'find-lisp-find-dired)

;;;; OS -- macOS

(when (eq system-type 'darwin)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;;; Font

;; x-list-fonts errors in terminal when no display available
(cond ((and (display-graphic-p) (x-list-fonts "Hack Nerd Font"))
       (add-to-list 'default-frame-alist '(font . "Hack Nerd Font-12"))))

;;;; Theme

(if (version< emacs-version "30.0")
    (load-theme 'modus-vivendi)
  (load-theme 'modus-vivendi-tinted t)
  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  (global-set-key (kbd "C-c m") #'modus-themes-toggle))

;;;; Built-in Settings

(setq ring-bell-function 'ignore)
(server-start)
(global-so-long-mode t)

;;;;; Unicode

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;; Mode line

(setq column-number-mode t
      mode-line-in-non-selected-windows t)

;;;;; Minibuffer history

(savehist-mode 1)

;;;;; ANSI colors

(defun renz/display-ansi-colors ()
  "Render colors in a buffer that contains ASCII color escape codes."
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook #'renz/display-ansi-colors)
(add-hook 'eshell-preoutput-filter-functions  #'ansi-color-apply)

;;;;; Recent files

(recentf-mode t)

(defun renz/find-recent-file ()
  "Find a file that was recently visted using `completing-read'."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))

;;;;; Display

(setq-default fill-column 80)
(scroll-bar-mode -1)
(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)
(setq-default indent-tabs-mode nil)
(setq mouse-wheel-tilt-scroll t)
(setq switch-to-buffer-obey-display-actions t)
(global-auto-revert-mode)

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)

;;;;; Diagnostics

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;;;; Editing defaults

(add-hook 'prog-mode-hook 'electric-pair-local-mode)
(add-hook 'prog-mode-hook 'show-paren-local-mode)
(setq use-short-answers t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(delete 'process-kill-buffer-query-function kill-buffer-query-functions)

;;;;; Line wrapping and numbers

(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))
(add-hook 'shell-mode-hook (lambda () (setq-local truncate-lines nil)))

(defun renz/display-relative-lines ()
  (setq display-line-numbers-width 3)
  (setq display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'renz/display-relative-lines)
(add-hook 'yaml-mode-hook #'renz/display-relative-lines)
(add-hook 'text-mode-hook #'renz/display-relative-lines)

;;;;; Selection and mouse

(delete-selection-mode t)
(xterm-mouse-mode 1)

;;;;; Compilation

(setq compilation-scroll-output 'first-error)

;;;;; Bars and menus

(tool-bar-mode -1)
(when (renz/windowsp)
  (menu-bar-mode -1))

;;;;; Dir-locals

;; Accept all dir-locals without prompting
(advice-add 'risky-local-variable-p :override #'ignore)

;;;;; Grep and find

(use-package grep
  :bind ("C-c g" . grep-find)
  :config
  (when (and (executable-find "rg") (renz/windowsp))
    (grep-apply-setting 'grep-find-command
                        '("rg --vimgrep --color never --ignore-case  ." . 42))))

(when-let ((on-windows (renz/windowsp))
           (prog-files (getenv "PROGRAMFILES(x86)"))
           (find-prg (expand-file-name "GnuWin32/bin/find.exe" prog-files))
           (find-exists (executable-find find-prg)))
  (setq find-program "C:\\\"Program Files (x86)\"\\GnuWin32\\bin\\find.exe"))

;;;;; Misc built-ins

(setq confirm-kill-emacs 'yes-or-no-p)

(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(cond ((executable-find "aspell")
       (setq ispell-program-name "aspell"
             ispell-really-aspell t))
      ((executable-find "hunspell")
       (setq ispell-program-name "hunspell"
             ispell-really-hunspell t)))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default mark-ring-max 32)
(setq global-mark-ring-max 32)

;;;;; Register bookmarks

(set-register ?S '(buffer . "*scratch*"))
(set-register ?I `(file . ,(expand-file-name "init.el" user-emacs-directory)))
(set-register ?B `(file . "~/.bashrc"))
(set-register ?P `(file . "~/.profile"))
(set-register ?C `(file . "~/.ssh/config"))

;;;;; Eldoc

(setq eldoc-echo-area-use-multiline-p nil)

;;;;; Imenu

(use-package imenu
  :config
  (setq imenu-auto-rescan t
        org-imenu-depth 3)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local imenu-generic-expression
                          (append '((nil "^;;;+ \\(.+\\)$" 1))
                                  imenu-generic-expression)))))

;;;;; Dabbrev and hippie-expand

(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package hippie-exp
  :config
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (delete 'try-expand-line hippie-expand-try-functions-list)
  (delete 'try-complete-lisp-symbol-partially hippie-expand-try-functions-list)
  (delete 'try-complete-lisp-symbol hippie-expand-try-functions-list))

;;;;; Dired

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-alFh")
  (setq dired-dwim-target t))

;;;;; eww

(use-package eww
  :config (setq eww-search-prefix "https://duckduckgo.com/html/?q="))

;;;;; Eglot

(use-package eglot
  :bind
  (("C-c l c" . eglot-reconnect)
   ("C-c l d" . flymake-show-buffer-diagnostics)
   ("C-c l f f" . eglot-format)
   ("C-c l f b" . eglot-format-buffer)
   ("C-c l l" . eglot)
   ("C-c l r n" . eglot-rename)
   ("C-c l s" . eglot-shutdown)
   ("C-c l i" . eglot-inlay-hints-mode))
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) "uv" "run" "ty" "server")))

;;;;; Async shell ^M fix

;; comint-output-filter handles carriage returns for progress bars/spinners
(defun renz/async-shell-command-filter-hook ()
  "Filter async shell command output via `comint-output-filter'."
  (when (equal (buffer-name (current-buffer)) "*Async Shell Command*")
    (setq-local comint-inhibit-carriage-motion nil)
    (when-let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc 'comint-output-filter))))

(add-hook 'shell-mode-hook #'renz/async-shell-command-filter-hook)

;;;;; Uniquify, save-place, clipboard

(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))

(save-place-mode 1)
(setq save-interprogram-paste-before-kill t)
(setq apropos-do-all t)
(setq frame-inhibit-implied-resize t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq load-prefer-newer t)

;;;;; Scripts directory

(defun renz/add-relative-to-exec-path (dir)
  "Add DIR relative to user Emacs configuration directory to 'exec-path'."
  (let ((dir-path (expand-file-name dir user-emacs-directory)))
    (mkdir dir-path t)
    (add-to-list 'exec-path dir-path)))

(renz/add-relative-to-exec-path "scripts")

;;;;; Editorconfig

(editorconfig-mode t)

;;;; Keybindings

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-z") #'zap-up-to-char)
(global-set-key [remap list-buffers] 'ibuffer)

(use-package flymake
  :bind (:map flymake-mode-map
         ("C-c n" . flymake-goto-next-error)
         ("C-c p" . flymake-goto-prev-error)))

;;;;; C-c bindings

(global-set-key (kbd "C-c b") #'compile)
(global-set-key (kbd "C-c B") #'recompile)

(defun renz/insert-current-dir ()
  "Insert the current `default-directory' at point."
  (interactive)
  (insert default-directory))

(defun renz/insert-current-file ()
  "Insert the current buffer's full file name at point."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(global-set-key (kbd "C-c c d") #'renz/insert-current-dir)
(global-set-key (kbd "C-c c f") #'renz/insert-current-file)
(global-set-key (kbd "C-c d") #'delete-pair)
(setq delete-pair-blink-delay 0.0)
(global-set-key (kbd "C-c i") #'browse-url-of-buffer)
(global-set-key (kbd "C-c k") #'bury-buffer)
(global-set-key (kbd "C-c q") #'replace-regexp)
(global-set-key (kbd "C-c r") #'renz/find-recent-file)
(global-set-key (kbd "C-c t") #'visit-tags-table)
(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c u") #'browse-url-at-point)

(defun renz/git-commit ()
  (interactive)
  (vc-next-action nil)
  (log-edit-show-diff)
  (other-window 1))

(global-set-key (kbd "C-c v") #'renz/git-commit)
(global-set-key (kbd "C-c w") #'whitespace-mode)
(global-set-key (kbd "C-c x r") #'restart-emacs)
(global-set-key (kbd "C-c <DEL>") #'backward-kill-sexp)
(global-set-key (kbd "C-c <SPC>") #'mark-sexp)

;;;;; Super bindings

(global-set-key (kbd "s-p") #'project-switch-project)

;;;; Completion

;;;;; Completion style

(use-package orderless
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator "[ &.]"))

;;;;; *Completions* buffer

(setq completions-format 'one-column)

(unless (version< emacs-version "29.0")
  (setq completions-max-height 15))

(unless (version< emacs-version "29.0")
  (setq completion-auto-help 'always
        completion-auto-select 'second-tab
        completion-show-help nil
        completions-sort 'historical
        completions-header-format nil))

;;;;; Completion previews (Emacs 30+)

(unless (version< emacs-version "30.0")
  (global-completion-preview-mode)
  (define-key completion-preview-active-mode-map (kbd "M-p") #'completion-preview-prev-candidate)
  (define-key completion-preview-active-mode-map (kbd "M-n") #'completion-preview-next-candidate)
  (define-key completion-preview-active-mode-map (kbd "M-f") #'completion-preview-insert-word)
  (define-key completion-preview-active-mode-map (kbd "C-M-f") #'completion-preview-insert-sexp))

;;;;; Corfu

(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match 'separator)
  (global-corfu-modes '((not shell-mode) (not eshell-mode) t))
  :config
  (global-corfu-mode))

;;;;; Vertico

(use-package vertico
  :config
  (vertico-mode))

;;;;; Marginalia

(use-package marginalia
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;; Languages

;;;;; Shell

(defun renz/sh-indentation ()
  (setq tab-width 8))

(add-hook 'sh-mode-hook #'renz/sh-indentation)
(add-hook 'bash-ts-mode-hook #'renz/sh-indentation)

;;;;; HTML

(use-package sgml-mode
  :defer t
  :custom
  (electric-pair-local-mode nil)
  :config
  (let* ((p-tag-old (assoc "p" html-tag-alist))
         (p-tag-new `("p" \n ,(cdr (cdr p-tag-old)))))
    (add-to-list 'html-tag-alist p-tag-new)
    (add-to-list 'html-tag-alist '("code"))))

;;;;; CSS

(setq css-indent-offset 2)

;;;;; Org-mode

(setq renz/org-home "~/.emacs.d/org/")
(put 'org-publish-project-alist 'safe-local-variable #'listp)
(put 'org-html-validation-link 'safe-local-variable #'symbolp)
(put 'org-html-head-include-scripts 'safe-local-variable #'symbolp)
(put 'org-html-head-include-default-style 'safe-local-variable #'symbolp)
(put 'org-html-head 'safe-local-variable #'stringp)

(defun renz/list-files-with-absolute-path (directory)
  "Return a list of files in DIRECTORY with their absolute paths."
  (cl-remove-if-not #'file-regular-p (directory-files directory t ".*\.org$")))

(use-package org
  :hook
  ((org-mode . (lambda () (progn
                            (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors)
                            (setq indent-tabs-mode nil)))))

  :bind
  (("C-c o a" . org-agenda)
   ("C-c o k" . org-babel-remove-result)
   ("C-c o y" . ox-clip-image-to-clipboard))

  :custom
  (org-image-actual-width nil "Enable resizing of images")
  (org-agenda-files (renz/list-files-with-absolute-path renz/org-home) "Sources for Org agenda view")
  (org-html-htmlize-output-type nil "See C-h f org-html-htmlize-output-type")
  (org-confirm-babel-evaluate nil "Don't ask for confirmation when executing src blocks")
  (org-goto-interface 'outline-path-completion "Use completing-read for org-goto (C-c C-j, nicer than imenu)")
  (org-outline-path-complete-in-steps nil "Flatten the outline path, instead of completing hierarchically")
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)

  :config
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (shell . t)
     (R . t)
     (lisp . t))))

;;;;; SQL

(use-package sql
  :mode ("\\.sql\\'" . sql-mode))

(add-to-list 'auto-mode-alist '("\\.ddl\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.bql\\'" . sql-mode))

(defun renz/sql-mode-hook ()
  (setq tab-width 4))

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

;;;;; BigQuery

(use-package bq
  :load-path "site-lisp"
  :demand t)

(defun org-babel-execute:bq (orig-fun body params)
  (if (string-equal-ignore-case (cdr (assq :engine params)) "bq")
      (json-to-org-table-parse-json-string
       (org-babel-execute:shell (concat "bq query --format=json --nouse_legacy_sql '" body "'")
                                params))
    (org-babel-execute:sql body params)))

(advice-add 'org-babel-execute:sql :around #'org-babel-execute:bq)

;;;;; Python

(add-to-list 'auto-mode-alist '("Pipfile" . toml-ts-mode))
(add-to-list 'vc-directory-exclusion-list ".venv")

(add-hook 'python-mode-hook
          (lambda () (setq-local imenu-create-index-function
                                 'python-imenu-create-flat-index)))

(add-hook 'python-ts-mode-hook
          (lambda () (setq-local imenu-create-index-function
                                 'python-imenu-treesit-create-flat-index)))

(defun pyrightconfig-write (virtualenv)
  "Write a `pyrightconfig.json' file at the Git root of a project
with `venvPath' and `venv' set to the absolute path of
`virtualenv'.  When run interactively, prompts for a directory to
select."
  (interactive "DEnv: ")
  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))
    (with-temp-file out-file (insert out-contents))
    (message (concat "Configured `" out-file "` to use environment `" venv-dir))))

;; Pyright error format: /path/file.py:LINE:COL - error: message
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))

(use-package python
  :config
  (require 'eglot)
  (setq python-check-command "uv run ruff format && uv run ruff check --fix")
  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-ts-mode-hook #'flymake-mode))

(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
(put 'python-interpreter 'safe-local-variable #'stringp)

;;;;; Markdown

(when (and (not (executable-find "markdown")) (executable-find "markdown_py"))
  (setq markdown-command "markdown_py"))

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(setq markdown-fontify-code-blocks-natively t)

;;;;; CSV

(use-package csv-mode
  :mode "\\.csv\\'")

;;;;; JavaScript

(use-package js
  :config
  (setq js-mode-map (define-keymap "M-." #'xref-find-definitions))
  (setq js-ts-mode-map (copy-keymap js-mode-map))
  (add-hook 'js-mode-hook (lambda ()
                            (setq indent-tabs-mode t)
                            (setq tab-width 2)
                            (setq js-indent-level 2))))

;;;; Eshell

(use-package eshell
  :custom
  (eshell-visual-commands '("make" "vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm"))
  (eshell-visual-subcommands '(("git" "log" "diff" "show")
                               ("micromamba" "install" "update" "upgrade" "create" "run" "self-update")
                               ("mamba" "install" "update" "upgrade")
                               ("poetry" "install" "update" "upgrade")
                               ("docker" "build")
                               ("uv" "pip"))))

;;;; Tools

;;;;; Tree-sitter

(use-package treesit-auto
  :load-path "site-lisp/treesit-auto"
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs
   '(awk bash c css go html make markdown python r ruby rust toml yaml))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;; Cloud

(defun renz/glogin ()
  "Log in to GCP"
  (interactive)
  (shell-command "gcloud auth login --update-adc"))

(provide 'init.el)
;;; init.el ends here
