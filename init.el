;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: internal
;; URL: https://robbmann.io/

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; [[file:README.org::*Theme][Theme:1]]
(use-package ef-themes
  :ensure t

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
  (setq ef-themes-to-toggle '(ef-cherie ef-light))

  :config
  (load-theme 'ef-cherie :no-confirm))
;; Theme:1 ends here

;; [[file:README.org::*Custom][Custom:1]]
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))
;; Custom:1 ends here

;; [[file:README.org::*Packages][Packages:1]]
(eval-when-compile
  (package-autoremove)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-install-selected-packages)
  (require 'use-package))
;; Packages:1 ends here

;; [[file:README.org::*Packages][Packages:2]]
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
;; Packages:2 ends here

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
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
;; Expanded/better defaults:5 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:6]]
(define-key isearch-mode-map (kbd "<C-return>")
  (defun isearch-done-opposite (&optional nopush edit)
    "End current search in the opposite side of the match."
    (interactive)
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end (goto-char isearch-other-end))))
;; Expanded/better defaults:6 ends here

;; [[file:README.org::*C-c bindings][C-c bindings:1]]
;; (global-set-key (kbd "C-c a") #')
(global-set-key (kbd "C-c b") #'scroll-bar-mode)
;; C-c bindings:1 ends here

;; [[file:README.org::*=C-c c= change inner/outer][=C-c c= change inner/outer:1]]
(global-set-key (kbd "C-c c i") #'change-inner)
(global-set-key (kbd "C-c c o") #'change-outer)
;; =C-c c= change inner/outer:1 ends here

;; [[file:README.org::*=C-c d= jump to a tag][=C-c d= jump to a tag:1]]
(global-set-key (kbd "C-c d") #'renz/find-tag)
;; =C-c d= jump to a tag:1 ends here

;; [[file:README.org::*=C-c e=][=C-c e=:1]]
;; (global-set-key (kbd "C-c e") #')
;; =C-c e=:1 ends here

;; [[file:README.org::*=C-c f= hippie-expand][=C-c f= hippie-expand:1]]
(global-set-key (kbd "C-c f") #'hippie-expand)
;; =C-c f= hippie-expand:1 ends here

;; [[file:README.org::*=C-c g= find file at point][=C-c g= find file at point:1]]
(global-set-key (kbd "C-c g") #'ffap)  ; inspired by vim `gf`
;; =C-c g= find file at point:1 ends here

;; [[file:README.org::*=C-c h=][=C-c h=:1]]
(global-set-key (kbd "C-c h") #'consult-history)
;; =C-c h=:1 ends here

;; [[file:README.org::*=C-c i= jump to a header in my configuration][=C-c i= jump to a header in my configuration:1]]
(global-set-key (kbd "C-c i") #'renz/jump-init)
;; =C-c i= jump to a header in my configuration:1 ends here

;; [[file:README.org::*=C-c j= imenu][=C-c j= imenu:1]]
(global-set-key (kbd "C-c j") #'consult-imenu)  ; matches major modes that use C-c C-j
;; =C-c j= imenu:1 ends here

;; [[file:README.org::*=C-c k= kill all but one space][=C-c k= kill all but one space:1]]
(global-set-key (kbd "C-c k") #'just-one-space)
;; =C-c k= kill all but one space:1 ends here

;; [[file:README.org::*=C-c l= Everything LSP (eglot)][=C-c l= Everything LSP (eglot):1]]
(global-set-key (kbd "C-c l d") #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c l f f") #'eglot-format)
(global-set-key (kbd "C-c l f b") #'eglot-format-buffer)
(global-set-key (kbd "C-c l l") #'eglot)
(global-set-key (kbd "C-c l r n") #'eglot-rename)
(global-set-key (kbd "C-c l s") #'eglot-shutdown)
;; =C-c l= Everything LSP (eglot):1 ends here

;; [[file:README.org::*=C-c m= toggle ef-theme][=C-c m= toggle ef-theme:1]]
(global-set-key (kbd "C-c m") #'ef-themes-toggle)
;; =C-c m= toggle ef-theme:1 ends here

;; [[file:README.org::*=C-c o= Org bindings][=C-c o= Org bindings:1]]
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o b d") #'org-babel-detangle)
(global-set-key (kbd "C-c o b o") #'org-babel-tangle-jump-to-org)
(global-set-key (kbd "C-c o b s") #'renz/org-babel-tangle-jump-to-src)
(global-set-key (kbd "C-c o j") #'consult-org-heading)
(global-set-key (kbd "C-c o k") #'org-babel-remove-result)
(global-set-key (kbd "C-c o o") #'renz/jump-org)
(global-set-key (kbd "C-c o w") #'renz/org-kill-src-block)
(global-set-key (kbd "C-c o y") #'ox-clip-image-to-clipboard)
;; =C-c o= Org bindings:1 ends here

;; [[file:README.org::*=C-c p= blacken-mode][=C-c p= blacken-mode:1]]
(global-set-key (kbd "C-c p") #'blacken-mode)
;; =C-c p= blacken-mode:1 ends here

;; [[file:README.org::*=C-c q= replace regexp][=C-c q= replace regexp:1]]
(global-set-key (kbd "C-c q") #'replace-regexp)
;; =C-c q= replace regexp:1 ends here

;; [[file:README.org::*=C-c r= recent files][=C-c r= recent files:1]]
(global-set-key (kbd "C-c r") #'consult-recent-file)
;; =C-c r= recent files:1 ends here

;; [[file:README.org::*=C-c s= shell][=C-c s= shell:1]]
(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'ansi-term)
(global-set-key (kbd "C-c s v") #'vterm)
;; =C-c s= shell:1 ends here

;; [[file:README.org::*=C-c t=][=C-c t=:1]]
;; (global-set-key (kbd "C-c t") #')
;; =C-c t=:1 ends here

;; [[file:README.org::*=C-c u= Consult grep/rg][=C-c u= Consult grep/rg:1]]
(global-set-key (kbd "C-c u") #'renz/consult-grep)
;; =C-c u= Consult grep/rg:1 ends here

;; [[file:README.org::*=C-c v= Consult line][=C-c v= Consult line:1]]
(global-set-key (kbd "C-c v") #'consult-line)
;; =C-c v= Consult line:1 ends here

;; [[file:README.org::*=C-c w=][=C-c w=:1]]
(global-set-key (kbd "C-c w") #'whitespace-mode)
;; =C-c w=:1 ends here

;; [[file:README.org::*=C-c x=][=C-c x=:1]]
;; (global-set-key (kbd "C-c x") #')
;; =C-c x=:1 ends here

;; [[file:README.org::*=C-c y= Yank inner/outer][=C-c y= Yank inner/outer:1]]
(global-set-key (kbd "C-c y i") #'copy-inner)
(global-set-key (kbd "C-c y o") #'copy-outer)
;; =C-c y= Yank inner/outer:1 ends here

;; [[file:README.org::*=C-c z=][=C-c z=:1]]
;; (global-set-key (kbd "C-c z") #')
;; =C-c z=:1 ends here

;; [[file:README.org::*=C-c= Other bindings][=C-c= Other bindings:1]]
(global-set-key (kbd "C-c ;") #'comment-line)  ; TTY-friendly
(global-set-key (kbd "C-c <DEL>") #'backward-kill-sexp)  ;; TTY-frindly
;; =C-c= Other bindings:1 ends here

;; [[file:README.org::*Meta/Alt Modifications][Meta/Alt Modifications:1]]
(with-eval-after-load 'dired
(define-key dired-mode-map (kbd "M-S-j") 'dired-goto-file))
;; Meta/Alt Modifications:1 ends here

;; [[file:README.org::*F5-F9][F5-F9:1]]
(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "M-<f5>") #'recompile)
;; (global-set-key (kbd "<f6>") #')
;; (global-set-key (kbd "M-<f6>") #')
;; (global-set-key (kbd "<f7>") #')
;; (global-set-key (kbd "M-<f7>") #')
;; (global-set-key (kbd "<f8>") #')
;; (global-set-key (kbd "M-<f8>") #')
;; (global-set-key (kbd "<f9>") #'vterm)
(global-set-key (kbd "M-<f9>") #'eshell)
(global-set-key (kbd "S-<f9>") #'ansi-term)
(global-set-key (kbd "s-<f9>") #'shell)
;; F5-F9:1 ends here

;; [[file:README.org::*Super bindings][Super bindings:1]]
(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-q") #'save-buffers-kill-terminal)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-v") #'yank)
;; Super bindings:1 ends here

;; [[file:README.org::*Mode line][Mode line:1]]
(setq column-number-mode t
      mode-line-in-non-selected-windows t
      display-battery-mode t
      display-time-day-and-date t)

(display-time)
;; Mode line:1 ends here

;; [[file:README.org::*=eldoc=][=eldoc=:1]]
(setq eldoc-echo-area-use-multiline-p nil)
;; =eldoc=:1 ends here

;; [[file:README.org::*Magit][Magit:1]]
(use-package magit
  :bind ("C-x g" . magit-status))
;; Magit:1 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:1]]
(setq completion-styles '(flex basic partial-completion emacs22))
;; Autocompletion:1 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:2]]
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 20)
;; Autocompletion:2 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:3]]
(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless)

  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; Autocompletion:3 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:4]]
(setq completions-format 'one-column)
;; Autocompletion:4 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:5]]
(defun renz/completion-accept ()
  "Expand current text to first completion result"
  (interactive)
  ;; FIXME In python REPL, if we go back inside a symbol and edit it
  ;;       to narrow the candidate list, then accept something with
  ;;       this function, the trailing text isn't erased
  (switch-to-completions)
  (choose-completion))

(defun renz/jump-completion ()
  "Jump to second completion."
  (interactive)
  (switch-to-completions)
  (next-completion 1))

(defun renz/completion-kill-completion-buffer ()
  "Close the *Completions* buffer without switching to it"
  (interactive)
  (kill-buffer "*Completions*"))
;; Autocompletion:5 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:6]]
(define-key completion-in-region-mode-map (kbd "C-n") 'renz/jump-completion)
(define-key completion-list-mode-map (kbd "C-n") 'next-completion)
(define-key completion-list-mode-map (kbd "C-p") 'previous-completion)
;; Autocompletion:6 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:7]]
(define-key completion-in-region-mode-map (kbd "C-j") 'renz/completion-accept)
(define-key completion-list-mode-map (kbd "C-j") 'choose-completion)
;; Autocompletion:7 ends here

;; [[file:README.org::*Minibuffer completion with ~vertico~][Minibuffer completion with ~vertico~:1]]
(use-package vertico
  :config
  (vertico-mode))
;; Minibuffer completion with ~vertico~:1 ends here

;; [[file:README.org::*Minibuffer completion with ~vertico~][Minibuffer completion with ~vertico~:2]]
(setq tab-always-indent 'complete)
;; Minibuffer completion with ~vertico~:2 ends here

;; [[file:README.org::*Completion at point with ~corfu~][Completion at point with ~corfu~:1]]
(use-package corfu-terminal
  :unless window-system
  :config
  (corfu-terminal-mode +1))

(use-package corfu
  :demand t

  :custom
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection

  :bind
  (:map corfu-map
	("M-SPC" . corfu-insert-separator)
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))

  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
		  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (setq corfu-auto t
	corfu-auto-delay 0.0
	corfu-quit-no-match 'separator)

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (advice-add #'corfu-insert :after #'corfu-send-shell)

  (global-corfu-mode))
;; Completion at point with ~corfu~:1 ends here

;; [[file:README.org::*Completion at point with ~corfu~][Completion at point with ~corfu~:2]]
(defun renz/disable-corfu-remote ()
  (when (and (fboundp 'corfu-mode)
	     (file-remote-p default-directory))
    (corfu-mode -1)))
;; Completion at point with ~corfu~:2 ends here

;; [[file:README.org::*~dabbrev~ adjustments for corfu][~dabbrev~ adjustments for corfu:1]]
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
;; ~dabbrev~ adjustments for corfu:1 ends here

;; [[file:README.org::*In case of emergency: ~fzf~][In case of emergency: ~fzf~:1]]
(use-package fzf
  :bind (("C-c f" . fzf))
  :config
  ;; (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"
	fzf/executable "fzf"
	fzf/git-grep-args "-i --line-number %s"
	;; command used for `fzf-grep-*` functions
	;; example usage for ripgrep:
	;; fzf/grep-command "rg --no-heading -nH"
	fzf/grep-command "grep -nrH"
	;; If nil, the fzf buffer will appear at the top of the window
	fzf/position-bottom t
	fzf/window-height 15))
;; In case of emergency: ~fzf~:1 ends here

;; [[file:README.org::*Org-mode][Org-mode:1]]
(setq renz/org-home "~/org/")
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
;; Org-mode:1 ends here

;; [[file:README.org::*Org-mode][Org-mode:3]]
(setq org-image-actual-width nil)
;; Org-mode:3 ends here

;; [[file:README.org::*~ox-hugo~ for exporting my blog][~ox-hugo~ for exporting my blog:1]]
(use-package ox-hugo
  :after (org))
;; ~ox-hugo~ for exporting my blog:1 ends here

;; [[file:README.org::*Jumping to org sources][Jumping to org sources:1]]
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
;; Jumping to org sources:1 ends here

;; [[file:README.org::*Jumping to org sources][Jumping to org sources:2]]
(use-package org
  :mode ("\\.org\\'" . org-mode)

  :hook
  (org-mode . (lambda () (progn
			   (add-hook 'after-save-hook #'org-babel-tangle :append :local)
			   (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors))))
  :hook (org-mode . visual-line-mode)

  :config
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (shell . t)
     ;; (fortran . t)
     ;; (julia . t)
     ;; (jupyter . t)
     ;; (scheme . t)
     ;; (haskell . t)
     ;; (lisp . t)
     ;; (clojure . t)
     ;; (C . t)
     ;; (org . t)
     ;; (gnuplot . t)
     ;; (awk . t)
     ;; (latex . t))
     )
   ;; Outside the typical TODO/DONE states, I like to use DEAD as an indicator
   ;; that something is fully blocked, but not done.
   (setq org-todo-keywords '((sequence "TODO" "DEAD" "DONE")))
   (setq org-agenda-files '("~/.emacs.d/org/work.org")
	 org-hugo-front-matter-format "yaml")))
;; Jumping to org sources:2 ends here

;; [[file:README.org::*=org-modern=][=org-modern=:1]]
(use-package org-modern
  :after (org)
  :config
  (setq
   ;; Edit settings
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

  (global-org-modern-mode))
;; =org-modern=:1 ends here

;; [[file:README.org::*Code block syntax highlighting for HTML export][Code block syntax highlighting for HTML export:1]]
(use-package htmlize
  :after (org))
;; Code block syntax highlighting for HTML export:1 ends here

;; [[file:README.org::*Copying images out of org-babel][Copying images out of org-babel:1]]
(use-package ox-clip
  :after (org))
;; Copying images out of org-babel:1 ends here

(provide 'init.el)
;;; init.el ends here
