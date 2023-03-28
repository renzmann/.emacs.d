;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: internal
;; URL: https://robbmann.io/

;;; Commentary:
;; A mostly minimal, reproducible Emacs configuration.  This file is
;; automatically tangled from README.org, with header/footer comments on each
;; code block that allow for de-tangling the source back to README.org when
;; working on this file directly.

;;; Code:

;; [[file:README.org::*Custom][Custom:1]]
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))
;; Custom:1 ends here

;; [[file:README.org::*Packages][Packages:1]]
(require 'package)
(setq package-enable-at-startup nil)
;; Packages:1 ends here

;; [[file:README.org::*Packages][Packages:2]]
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Packages:2 ends here

;; [[file:README.org::*Packages][Packages:3]]
(defun renz/package-sync ()
  "Remove unused sources and install any missing ones."
  (interactive)
  (package-autoremove)
  (package-install-selected-packages))
;; Packages:3 ends here

;; [[file:README.org::*Packages][Packages:4]]
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
;; Packages:4 ends here

;; [[file:README.org::*Proxy settings][Proxy settings:2]]
(let ((proxy-file (expand-file-name "proxy.el" user-emacs-directory)))
  (when (file-exists-p proxy-file)
    (load-file proxy-file)))
;; Proxy settings:2 ends here

;; [[file:README.org::*Configuration][Configuration:1]]
(defun renz/windowsp ()
  "Are we on Microsoft Windows?"
  (memq system-type '(windows-nt cygwin ms-dos)))

(when (renz/windowsp)
  ;; Set a font that supports emoji
  (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji") nil 'prepend)
  (set-face-attribute 'default nil :font "Hack NF-12")

  ;; Alternate ispell when we've got msys on Windows
  (setq ispell-program-name "aspell.exe"))
;; Configuration:1 ends here

;; [[file:README.org::*Configuration][Configuration:1]]
(when (eq system-type 'darwin)
  ;; Uncomment this if we can't install Hack Nerd font
  ;; (set-face-attribute 'default nil :font "Menlo-14")
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-13")
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))
;; Configuration:1 ends here

;; [[file:README.org::*Theme][Theme:1]]
(use-package ef-themes
  :if (display-graphic-p)
  :demand t
  :bind ("C-c m" . ef-themes-toggle)

  :init
  (setq ef-themes-headings
        '((0 . (1.9))
          (1 . (1.3))
          (2 . (1.2))
          (3 . (1.1))
          (4 . (1.0))
          (5 . (1.0)) ; absence of weight means `bold'
          (6 . (1.0))
          (7 . (1.0))
          (t . (1.0))))
  (setq ef-themes-to-toggle '(ef-cherie ef-summer))

  :config
  (load-theme 'ef-cherie :no-confirm))
;; Theme:1 ends here

;; [[file:README.org::*Start a server for =emacsclient=][Start a server for =emacsclient=:1]]
(server-start)
;; Start a server for =emacsclient=:1 ends here

;; [[file:README.org::*Unicode][Unicode:1]]
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; Unicode:1 ends here

;; [[file:README.org::*Mode line][Mode line:2]]
(setq column-number-mode t
      mode-line-in-non-selected-windows t)
;; Mode line:2 ends here

;; [[file:README.org::*Remember minibuffer history][Remember minibuffer history:1]]
(setq history-length 25)
(savehist-mode 1)
;; Remember minibuffer history:1 ends here

;; [[file:README.org::*Colored output in ~eshell~][Colored output in ~eshell~:1]]
(add-hook 'eshell-preoutput-filter-functions  'ansi-color-apply)
;; Colored output in ~eshell~:1 ends here

;; [[file:README.org::*Recent files menu][Recent files menu:1]]
(recentf-mode t)

(defun renz/find-recent-file ()
  "Find a file that was recently visted using `completing-read'."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))
;; Recent files menu:1 ends here

;; [[file:README.org::*Fill-column][Fill-column:1]]
(setq-default fill-column 80)
;; Fill-column:1 ends here

;; [[file:README.org::*Scroll bar][Scroll bar:1]]
(scroll-bar-mode -1)
;; Scroll bar:1 ends here

;; [[file:README.org::*Window margins and fringe][Window margins and fringe:1]]
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(set-face-background 'fringe (face-attribute 'default :background))
;; Window margins and fringe:1 ends here

;; [[file:README.org::*Automatically visit symlink sources][Automatically visit symlink sources:1]]
(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)
;; Automatically visit symlink sources:1 ends here

;; [[file:README.org::*Indent with spaces by default][Indent with spaces by default:1]]
(setq-default indent-tabs-mode nil)
;; Indent with spaces by default:1 ends here

;; [[file:README.org::*Indent with spaces by default][Indent with spaces by default:2]]
(add-hook 'sh-mode-hook (lambda () (setq indent-tabs-mode t)))
;; Indent with spaces by default:2 ends here

;; [[file:README.org::*Render ASCII color escape codes][Render ASCII color escape codes:1]]
(defun renz/display-ansi-colors ()
  "Render colors in a buffer that contains ASCII color escape codes."
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))
;; Render ASCII color escape codes:1 ends here

;; [[file:README.org::*Enable horizontal scrolling with mouse][Enable horizontal scrolling with mouse:1]]
(setq mouse-wheel-tilt-scroll t)
;; Enable horizontal scrolling with mouse:1 ends here

;; [[file:README.org::*Window management][Window management:1]]
(unless (version< emacs-version "27.1")
  (setq switch-to-buffer-obey-display-actions t))
;; Window management:1 ends here

;; [[file:README.org::*Automatically update buffers when contents change on disk][Automatically update buffers when contents change on disk:1]]
(global-auto-revert-mode)
;; Automatically update buffers when contents change on disk:1 ends here

;; [[file:README.org::*Highlight the line point is on][Highlight the line point is on:1]]
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)
;; Highlight the line point is on:1 ends here

;; [[file:README.org::*Always turn on flymake in prog mode][Always turn on flymake in prog mode:1]]
(add-hook 'prog-mode-hook #'flymake-mode)
;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; Always turn on flymake in prog mode:1 ends here

;; [[file:README.org::*Stop stupid bell][Stop stupid bell:1]]
;; Stop stupid bell
(setq ring-bell-function 'ignore)
;; Stop stupid bell:1 ends here

;; [[file:README.org::*Automatically create matching parens in programming modes][Automatically create matching parens in programming modes:1]]
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))
;; Automatically create matching parens in programming modes:1 ends here

;; [[file:README.org::*Shorten yes/no prompts to y/n][Shorten yes/no prompts to y/n:1]]
(setq use-short-answers t)
;; Shorten yes/no prompts to y/n:1 ends here

;; [[file:README.org::*Delete whitespace on save][Delete whitespace on save:1]]
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Delete whitespace on save:1 ends here

;; [[file:README.org::*Killing buffers with a running process][Killing buffers with a running process:1]]
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))
;; Killing buffers with a running process:1 ends here

;; [[file:README.org::*Don't wrap lines][Don't wrap lines:1]]
(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))
;; Don't wrap lines:1 ends here

;; [[file:README.org::*Relative line numbers][Relative line numbers:1]]
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))
(add-hook 'yaml-mode-hook (lambda () (setq display-line-numbers 'relative)))
(unless (display-graphic-p)
  (add-hook 'text-mode-hook (lambda () (setq display-line-numbers 'relative))))
;; Relative line numbers:1 ends here

;; [[file:README.org::*Delete region when we yank on top of it][Delete region when we yank on top of it:1]]
(delete-selection-mode t)
;; Delete region when we yank on top of it:1 ends here

;; [[file:README.org::*Enable mouse in terminal/TTY][Enable mouse in terminal/TTY:1]]
(xterm-mouse-mode 1)
;; Enable mouse in terminal/TTY:1 ends here

;; [[file:README.org::*Compilation][Compilation:1]]
(setq compilation-scroll-output t)
;; Compilation:1 ends here

;; [[file:README.org::*Compilation][Compilation:2]]
(defun renz/colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'renz/colorize-compilation-buffer)
;; Compilation:2 ends here

;; [[file:README.org::*Tool bar][Tool bar:1]]
(tool-bar-mode -1)
;; Tool bar:1 ends here

;; [[file:README.org::*Tool bar][Tool bar:2]]
(when (renz/windowsp)
  (menu-bar-mode -1))
;; Tool bar:2 ends here

;; [[file:README.org::*Ignore risky .dir-locals.el][Ignore risky .dir-locals.el:1]]
(advice-add 'risky-local-variable-p :override #'ignore)
;; Ignore risky .dir-locals.el:1 ends here

;; [[file:README.org::*Prefer =rg= over =grep=][Prefer =rg= over =grep=:1]]
(require 'grep)
(when (executable-find "rg")
  (setq grep-program "rg")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))
;; Prefer =rg= over =grep=:1 ends here

;; [[file:README.org::*Shorter file paths in grep/compilation buffers][Shorter file paths in grep/compilation buffers:1]]
(use-package scf-mode
  :load-path "site-lisp"
  :hook (grep-mode . (lambda () (scf-mode 1))))
;; Shorter file paths in grep/compilation buffers:1 ends here

;; [[file:README.org::*Confirm when exiting Emacs][Confirm when exiting Emacs:1]]
(setq confirm-kill-emacs 'yes-or-no-p)
;; Confirm when exiting Emacs:1 ends here

;; [[file:README.org::*Smooth scrolling][Smooth scrolling:1]]
(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))
;; Smooth scrolling:1 ends here

;; [[file:README.org::*Prefer ~aspell~ over ~ispell~][Prefer ~aspell~ over ~ispell~:1]]
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"))
;; Prefer ~aspell~ over ~ispell~:1 ends here

;; [[file:README.org::*Backup and auto-save files][Backup and auto-save files:1]]
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups/"))
      ;; auto-save-file-name-transforms
      ;; '(("." ,temporary-file-directory t))
      )
;; Backup and auto-save files:1 ends here

;; [[file:README.org::*Enable ~narrow-to-region~][Enable ~narrow-to-region~:1]]
(put 'narrow-to-region 'disabled nil)
;; Enable ~narrow-to-region~:1 ends here

;; [[file:README.org::*Enable up/downcase-region][Enable up/downcase-region:1]]
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Enable up/downcase-region:1 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:1]]
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)
;; Expanded/better defaults:1 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:2]]
(global-set-key (kbd "C-z") #'zap-up-to-char)
;; Expanded/better defaults:2 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:3]]
(global-set-key [remap dabbrev-expand] 'hippie-expand)
;; Expanded/better defaults:3 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:4]]
(global-set-key [remap list-buffers] 'ibuffer)
;; Expanded/better defaults:4 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:5]]
(use-package flymake
  :bind (:map flymake-mode-map
         ("C-c n" . flymake-goto-next-error)
         ("C-c p" . flymake-goto-prev-error)))
;; Expanded/better defaults:5 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:6]]
(define-key isearch-mode-map (kbd "<C-return>")
  (defun isearch-done-opposite (&optional nopush edit)
    "End current search in the opposite side of the match."
    (interactive)
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end (goto-char isearch-other-end))))
;; Expanded/better defaults:6 ends here

;; [[file:README.org::*Overriding defaults][Overriding defaults:1]]
(global-set-key (kbd "C-x C-p") 'previous-buffer)  ; Overrides `mark-page'
(global-set-key (kbd "C-x C-n") 'next-buffer)      ; Overrides `set-goal-column'
;; Overriding defaults:1 ends here

;; [[file:README.org::*=C-c b= build / compile][=C-c b= build / compile:1]]
(global-set-key (kbd "C-c b") #'compile)
(global-set-key (kbd "C-c B") #'recompile)
;; =C-c b= build / compile:1 ends here

;; [[file:README.org::*=C-c d= Navigating to symbols using old-school TAGS][=C-c d= Navigating to symbols using old-school TAGS:2]]
(defun renz/find-tag ()
  "Use `completing-read' to navigate to a tag."
  (interactive)
  (require 'etags)
  (tags-completion-table)
  (xref-find-definitions (completing-read "Find tag: " tags-completion-table)))

(global-set-key (kbd "C-c d") #'renz/find-tag)
;; =C-c d= Navigating to symbols using old-school TAGS:2 ends here

;; [[file:README.org::*=C-c f= find file at point (ffap)][=C-c f= find file at point (ffap):1]]
(global-set-key (kbd "C-c f") #'ffap)
;; =C-c f= find file at point (ffap):1 ends here

;; [[file:README.org::*=C-c i= jump to a header in my configuration][=C-c i= jump to a header in my configuration:1]]
(setq renz/site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))

(defun renz/--jump-section (dirname prompt extension)
  "Jump to a section of my configuration.
Asks for a file under `DIRNAME' using `PROMPT' in the user Emacs
config site with matching `EXTENSION' regexp."
  (find-file
   (concat dirname
           (completing-read prompt
                            (directory-files dirname nil extension)))))

(defun renz/jump-configuration ()
  "Prompt for a .el file in my site-lisp folder, then go there."
  (interactive)
  (renz/--jump-section renz/site-lisp-dir "Elisp config files: " ".*\.el$"))

(defun renz/jump-init ()
  "Jump directly to my literate configuration document."
  (interactive)
  (find-file (expand-file-name "README.org" user-emacs-directory)))

(global-set-key (kbd "C-c i i") #'renz/jump-init)
(global-set-key (kbd "C-c i l") #'renz/jump-configuration)
;; =C-c i= jump to a header in my configuration:1 ends here

;; [[file:README.org::*=C-c j= Toggle window split][=C-c j= Toggle window split:1]]
(defun toggle-window-split ()
  "Switch between horizontal and vertical split window layout."
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
;; =C-c j= Toggle window split:1 ends here

;; [[file:README.org::*=C-c k= kill all but one space][=C-c k= kill all but one space:1]]
(global-set-key (kbd "C-c k") #'just-one-space)
;; =C-c k= kill all but one space:1 ends here

;; [[file:README.org::*=C-c q= replace regexp][=C-c q= replace regexp:1]]
(global-set-key (kbd "C-c q") #'replace-regexp)
;; =C-c q= replace regexp:1 ends here

;; [[file:README.org::*=C-c r= find recent files][=C-c r= find recent files:1]]
(global-set-key (kbd "C-c r") #'renz/find-recent-file)
;; =C-c r= find recent files:1 ends here

;; [[file:README.org::*=C-c s= shell][=C-c s= shell:1]]
(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'term)
;; =C-c s= shell:1 ends here

;; [[file:README.org::*=C-c v= open thing at point in browser][=C-c v= open thing at point in browser:1]]
(global-set-key (kbd "C-c v") #'browse-url-at-point)
;; =C-c v= open thing at point in browser:1 ends here

;; [[file:README.org::*=C-c w= whitespace mode][=C-c w= whitespace mode:1]]
(global-set-key (kbd "C-c w") #'whitespace-mode)
;; =C-c w= whitespace mode:1 ends here

;; [[file:README.org::*=C-c= Other bindings][=C-c= Other bindings:1]]
(global-set-key (kbd "C-c <DEL>") #'backward-kill-sexp)  ;; TTY-frindly
(global-set-key (kbd "C-c <SPC>") #'mark-sexp)  ;; TTY-friendly
;; =C-c= Other bindings:1 ends here

;; [[file:README.org::*Completion style: Orderless][Completion style: Orderless:1]]
(setq completion-styles '(flex basic partial-completion emacs22))
;; Completion style: Orderless:1 ends here

;; [[file:README.org::*Completion style: Orderless][Completion style: Orderless:2]]
(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless)
  (setq orderless-component-separator "[ &]")

  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; Completion style: Orderless:2 ends here

;; [[file:README.org::*Nicer Display and Behavior of ~*Completions*~][Nicer Display and Behavior of ~*Completions*~:1]]
(setq completions-format 'one-column)
;; Nicer Display and Behavior of ~*Completions*~:1 ends here

;; [[file:README.org::*Nicer Display and Behavior of ~*Completions*~][Nicer Display and Behavior of ~*Completions*~:2]]
(unless (version< emacs-version "29.0")
  (setq completions-max-height 15))
;; Nicer Display and Behavior of ~*Completions*~:2 ends here

;; [[file:README.org::*Nicer Display and Behavior of ~*Completions*~][Nicer Display and Behavior of ~*Completions*~:3]]
(unless (version< emacs-version "29.0")
  (setq completion-auto-help 'visible
        completion-auto-select 'second-tab
        completion-show-help nil
        completions-sort nil
        completions-header-format nil))
;; Nicer Display and Behavior of ~*Completions*~:3 ends here

;; [[file:README.org::*Nicer Display and Behavior of ~*Completions*~][Nicer Display and Behavior of ~*Completions*~:4]]
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

(unless (version< emacs-version "29.0")
  (setq completions-sort #'renz/sort-multi-category))
;; Nicer Display and Behavior of ~*Completions*~:4 ends here

;; [[file:README.org::*Completion at point][Completion at point:1]]
(setq tab-always-indent 'complete)
;; Completion at point:1 ends here

;; [[file:README.org::*GUI Emacs: corfu and vertico][GUI Emacs: corfu and vertico:1]]
(use-package corfu
  :load-path "site-lisp/corfu"
  :demand t
  :bind
  (:map corfu-map
	;; ("SPC" . corfu-insert-separator)
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (completion-styles '(orderless-fast))
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (global-corfu-mode))

(use-package vertico
  :load-path "site-lisp/vertico"
  :config
  (vertico-mode))
;; GUI Emacs: corfu and vertico:1 ends here

;; [[file:README.org::*Shell (Bash, sh, ...)][Shell (Bash, sh, ...):1]]
(defun renz/sh-indentation ()
  (setq indent-tabs-mode t)
  (setq tab-width 8))

(add-hook 'sh-mode-hook #'renz/sh-indentation)
;; Shell (Bash, sh, ...):1 ends here

;; [[file:README.org::*CSS][CSS:1]]
(setq css-indent-offset 2)
;; CSS:1 ends here

;; [[file:README.org::*Org-mode][Org-mode:1]]
(setq renz/org-home "~/org/")
;; Org-mode:1 ends here

;; [[file:README.org::*Org-mode][Org-mode:2]]
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
;; Org-mode:2 ends here

;; [[file:README.org::*Org-mode][Org-mode:3]]
(use-package org
  :hook
  ((org-mode . (lambda () (progn
                            (add-hook 'after-save-hook #'org-babel-tangle :append :local)
                            (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors)
                            (setq indent-tabs-mode t)))))

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
   ("C-c o k" . org-babel-remove-result)
   ("C-c o o" . renz/jump-org)
   ("C-c o w" . renz/org-kill-src-block)
   ("C-c o y" . ox-clip-image-to-clipboard))

  :custom
  (org-image-actual-width nil "Enable resizing of images")
  (org-agenda-files (list (expand-file-name "work.org" renz/org-home)) "Sources for Org agenda view")
  (org-html-htmlize-output-type nil "See C-h f org-html-htmlize-output-type")
  (org-confirm-babel-evaluate nil "Don't ask for confirmation when executing src blocks")
  (org-edit-src-content-indentation 2 "Indent all src blocks by this much")
  (org-goto-interface 'outline-path-completion "Use completing-read for org-goto (C-c C-j, nicer than imenu)")
  (org-outline-path-complete-in-steps nil "Flatten the outline path, instead of completing hierarchically")

:config
(add-to-list 'org-modules 'org-tempo)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (sql . t)
   (shell . t)
   (R . t)
   ;; (fortran . t)
   ;; (julia . t)
   ;; (jupyter . t)
   ;; (scheme . t)
   ;; (haskell . t)
   (lisp . t)
   ;; (clojure . t)
   ;; (C . t)
   ;; (org . t)
   ;; (gnuplot . t)
   ;; (awk . t)
   ;; (latex . t)
   )))
;; Org-mode:3 ends here

;; [[file:README.org::*=org-modern=][=org-modern=:1]]
(use-package org-modern
  :after org
  :config
  (setq org-auto-align-tags nil
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
        org-agenda-time-grid '((daily today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "<─ now ────────────────────────────────────────────────")

  (if (display-graphic-p)
      (setq org-modern-table t)
    (setq org-modern-table nil))

  (global-org-modern-mode))
;; =org-modern=:1 ends here

;; [[file:README.org::*Copying images out of org-babel][Copying images out of org-babel:1]]
(use-package ox-clip
  :after org
  :config
  (setq org-hugo-front-matter-format "yaml"))
;; Copying images out of org-babel:1 ends here

;; [[file:README.org::*Exporting to Hugo][Exporting to Hugo:1]]
(use-package ox-hugo
  :after org)
;; Exporting to Hugo:1 ends here

;; [[file:README.org::*Converting JSON to Org Tables][Converting JSON to Org Tables:1]]
(use-package json-to-org-table
  :load-path "site-lisp/json-to-org-table/"
  :after org)
;; Converting JSON to Org Tables:1 ends here

;; [[file:README.org::*Indentation][Indentation:2]]
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
;; Indentation:2 ends here

;; [[file:README.org::*Interactive ~hive2~ mode][Interactive ~hive2~ mode:1]]
(use-package hive2
  :load-path "site-lisp/"
  :demand t
  :mode ("\\.hql" . sql-mode))
;; Interactive ~hive2~ mode:1 ends here

;; [[file:README.org::*BigQuery ~sql~ Blocks in Org-Babel][BigQuery ~sql~ Blocks in Org-Babel:1]]
(defun org-babel-execute:bq (orig-fun body params)
  (if (string-equal-ignore-case (cdr (assq :engine params)) "bq")
      (json-to-org-table-parse-json-string
       (org-babel-execute:shell (concat "bq query --format=json --nouse_legacy_sql '" body "'")
                                params))
    (org-babel-execute:sql body params)))

(advice-add 'org-babel-execute:sql :around #'org-babel-execute:bq)
;; BigQuery ~sql~ Blocks in Org-Babel:1 ends here

;; [[file:README.org::*Pyright error links in =*compilation*=][Pyright error links in =*compilation*=:1]]
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))
;; Pyright error links in =*compilation*=:1 ends here

;; [[file:README.org::*Python check with "ruff"][Python check with "ruff":1]]
(use-package python
  :config
  (setq python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode))
;; Python check with "ruff":1 ends here

;; [[file:README.org::*Make check command and virtualenv root safe for .dir-locals.el][Make check command and virtualenv root safe for .dir-locals.el:2]]
(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
(put 'pyvenv-default-virtual-env-name 'safe-local-variable #'stringp)
;; Make check command and virtualenv root safe for .dir-locals.el:2 ends here

;; [[file:README.org::*pyrightconfig.json][pyrightconfig.json:1]]
(use-package pyrightconfig
  :after (python))
;; pyrightconfig.json:1 ends here

;; [[file:README.org::*Activating Virtual Environments Over Tramp][Activating Virtual Environments Over Tramp:1]]
(use-package tramp-venv
  :bind
  (("C-c t v a" . tramp-venv-activate)
   ("C-c t v d" . tramp-venv-deactivate)))
;; Activating Virtual Environments Over Tramp:1 ends here

;; [[file:README.org::*Pyvenv for virtual environments][Pyvenv for virtual environments:1]]
(use-package pyvenv
  :config
  (pyvenv-mode))
;; Pyvenv for virtual environments:1 ends here

;; [[file:README.org::*Executing cell-by-cell][Executing cell-by-cell:1]]
(use-package code-cells
  :hook ((python-mode . code-cells-mode-maybe)
         (python-ts-mode . code-cells-mode-maybe))
  :config
  (add-to-list 'code-cells-eval-region-commands '(python-ts-mode . python-shell-send-region)))
;; Executing cell-by-cell:1 ends here

;; [[file:README.org::*Markdown][Markdown:1]]
(defun renz/md-hook ()
  "View buffer in visual fill mode with 80 character width."
  (interactive)
  (visual-fill-column-mode)
  (setq-local fill-column 80))
;; Markdown:1 ends here

;; [[file:README.org::*Markdown][Markdown:2]]
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
;; Markdown:2 ends here

;; [[file:README.org::*Markdown][Markdown:3]]
(use-package poly-markdown
  :after (markdown-mode)
  :mode ("\\.md" . poly-markdown-mode)
  :config
  (setq markdown-command "markdown_py"))
;; Markdown:3 ends here

;; [[file:README.org::*Missing auto-modes][Missing auto-modes:1]]
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; Missing auto-modes:1 ends here

;; [[file:README.org::*csv-mode][csv-mode:1]]
(use-package csv-mode
  :mode "\\.csv\\'")
;; csv-mode:1 ends here

;; [[file:README.org::*=eldoc=][=eldoc=:1]]
(setq eldoc-echo-area-use-multiline-p nil)
;; =eldoc=:1 ends here

;; [[file:README.org::*~imenu~][~imenu~:1]]
(use-package imenu
  :config
  (setq imenu-auto-rescan t
        org-imenu-depth 3))
;; ~imenu~:1 ends here

;; [[file:README.org::*~dabbrev~: swap ~M-/~ and ~C-M-/~][~dabbrev~: swap ~M-/~ and ~C-M-/~:1]]
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
;; ~dabbrev~: swap ~M-/~ and ~C-M-/~:1 ends here

;; [[file:README.org::*dired][dired:1]]
(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-alFh")
  (setq dired-dwim-target t))
;; dired:1 ends here

;; [[file:README.org::*Coterm mode][Coterm mode:1]]
(use-package coterm
  :unless (renz/windowsp)
  :config
  (coterm-mode))
;; Coterm mode:1 ends here

;; [[file:README.org::*Visual fill column][Visual fill column:1]]
(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))
;; Visual fill column:1 ends here

;; [[file:README.org::*eww - search engine and browser][eww - search engine and browser:1]]
(use-package eww
  :config (setq eww-search-prefix "https://duckduckgo.com/html/?q="))
;; eww - search engine and browser:1 ends here

;; [[file:README.org::*Esup: startup time profiling][Esup: startup time profiling:1]]
(use-package esup
  :bind ("C-c x p")
  :config
  (setq esup-depth 0))
;; Esup: startup time profiling:1 ends here

;; [[file:README.org::*Reloading Emacs][Reloading Emacs:1]]
(use-package restart-emacs
  :bind ("C-c x r" . restart-emacs))
;; Reloading Emacs:1 ends here

;; [[file:README.org::*Language Server Protocol (LSP) with ~eglot~][Language Server Protocol (LSP) with ~eglot~:1]]
(use-package eglot
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)))
;; Language Server Protocol (LSP) with ~eglot~:1 ends here

;; [[file:README.org::*About TreeSitter and its Load Paths][About TreeSitter and its Load Paths:1]]
(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
  (add-to-list 'treesit-extra-load-path "~/.local/lib/"))
;; About TreeSitter and its Load Paths:1 ends here

;; [[file:README.org::*Automatically Using TreeSitter Modes][Automatically Using TreeSitter Modes:1]]
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))
;; Automatically Using TreeSitter Modes:1 ends here

;; [[file:README.org::*Ooo, aaah, shiny colors][Ooo, aaah, shiny colors:1]]
(setq-default treesit-font-lock-level 3)
;; Ooo, aaah, shiny colors:1 ends here

;; [[file:README.org::*Tramp][Tramp:1]]
(use-package tramp
  :defer t
  :config
  (setq vc-handled-backends '(Git)
        file-name-inhibit-locks t
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
;; Tramp:1 ends here

;; [[file:README.org::*Tramp][Tramp:2]]
(setq tramp-use-ssh-controlmaster-options nil)
;; Tramp:2 ends here

(provide 'init.el)
;;; init.el ends here
