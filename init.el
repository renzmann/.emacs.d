;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: internal
;; URL: https://robbmann.io/

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; [[file:README.org::*Custom][Custom:1]]
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))
;; Custom:1 ends here

;; [[file:README.org::*Packages][Packages:1]]
(eval-when-compile
  (package-autoremove)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))
;; Packages:1 ends here

;; [[file:README.org::*Packages][Packages:2]]
(add-to-list 'load-path (expand-file-name "packages/" user-emacs-directory))
;; Packages:2 ends here

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

;; [[file:README.org::*Keybound functions][Keybound functions:1]]
(defun renz/--jump-section (dirname prompt extension)
  "For internal use: prompt for a file under `dirname' in the user
emacs config site with matching `extension' regexp"
  (let ((complete-dir (concat user-emacs-directory dirname "/")))
    (find-file
     (concat complete-dir
             (completing-read prompt
                              (directory-files complete-dir nil extension))))))

(defun renz/jump-configuration ()
  "Prompt for a .el file in my configuration folder, then go there."
  (interactive)
  (renz/--jump-section "config" "Elisp config files: " ".*\.el$"))

;; FIXME: should set an org-home or something like that.  Probably a common variable
;; described somewhere in the org manual
(defun renz/jump-org ()
  "Prompt for an org file in my emacs directory, then go there."
  (interactive)
  (renz/--jump-section "org" "Org files: " ".*\.org$"))

(defun renz/jump-init ()
  (interactive)
  (find-file (expand-file-name "README.org" user-emacs-directory))
  (consult-org-heading))

(defun renz/find-tag ()
  "Use completing-read to navigate to a tag"
  (interactive)
  (xref-find-definitions (completing-read "Find tag: " tags-completion-table)))

(defun renz/consult-grep ()
  "Live grep using `rg' if found, otherwise `grep'"
  (interactive)
  (if (executable-find "rg")
      (consult-ripgrep)
    (consult-grep)))
;; Keybound functions:1 ends here

;; [[file:README.org::*Expanded defaults][Expanded defaults:1]]
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)
;; Expanded defaults:1 ends here

;; [[file:README.org::*Expanded defaults][Expanded defaults:2]]
(global-set-key (kbd "C-z") #'zap-up-to-char)
;; Expanded defaults:2 ends here

;; [[file:README.org::*Expanded defaults][Expanded defaults:4]]
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap switch-to-buffer] 'consult-buffer)
;; Expanded defaults:4 ends here

;; [[file:README.org::*Expanded defaults][Expanded defaults:5]]
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
;; Expanded defaults:5 ends here

;; [[file:README.org::*Expanded defaults][Expanded defaults:6]]
(define-key isearch-mode-map (kbd "<C-return>")
  (defun isearch-done-opposite (&optional nopush edit)
    "End current search in the opposite side of the match."
    (interactive)
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end (goto-char isearch-other-end))))
;; Expanded defaults:6 ends here

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
;; (global-set-key (kbd "C-c h") #')
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

;; [[file:README.org::*=C-c n= toggle minimap][=C-c n= toggle minimap:1]]
(global-set-key (kbd "C-c n") #'minimap-mode)
;; =C-c n= toggle minimap:1 ends here

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

;; [[file:README.org::*=C-c p=][=C-c p=:1]]
;; (global-set-key (kbd "C-c p") #')
;; =C-c p=:1 ends here

;; [[file:README.org::*=C-c q=][=C-c q=:1]]
;; (global-set-key (kbd "C-c q") #')
;; =C-c q=:1 ends here

;; [[file:README.org::*=C-c r= recent files][=C-c r= recent files:1]]
(global-set-key (kbd "C-c r") #'consult-recent-file)
;; =C-c r= recent files:1 ends here

;; [[file:README.org::*=C-c s= shell][=C-c s= shell:1]]
(global-set-key (kbd "C-c s s") #'shell)
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
;; (global-set-key (kbd "C-c w") #')
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
(global-set-key (kbd "<f9>") #'vterm)
(global-set-key (kbd "M-<f9>") #'eshell)
(global-set-key (kbd "S-<f9>") #'ansi-term)
(global-set-key (kbd "s-<f9>") #'shell)
;; F5-F9:1 ends here

;; [[file:README.org::*Super bindings][Super bindings:1]]
(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-v") #'yank)
;; Super bindings:1 ends here

;; [[file:README.org::*Consult][Consult:1]]
(use-package consult
  :ensure t)
;; Consult:1 ends here

;; [[file:README.org::*Fill-column][Fill-column:1]]
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq-default fill-column 120)
;; Fill-column:1 ends here

;; [[file:README.org::*Scroll bar][Scroll bar:1]]
;; Scroll bar
(scroll-bar-mode -1)
;; Scroll bar:1 ends here

;; [[file:README.org::*Inihibit splash screen][Inihibit splash screen:1]]
(setq inhibit-splash-screen t)
;; Inihibit splash screen:1 ends here

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

;; [[file:README.org::*Indent with spaces][Indent with spaces:1]]
(setq-default indent-tabs-mode nil)
;; Indent with spaces:1 ends here

;; [[file:README.org::*Render ASCII color escape codes][Render ASCII color escape codes:1]]
(defun renz/display-ansi-colors ()
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

;; [[file:README.org::*Enable up/downcase-region][Enable up/downcase-region:1]]
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Enable up/downcase-region:1 ends here

;; [[file:README.org::*Automatically update buffers when contents change on disk][Automatically update buffers when contents change on disk:1]]
(global-auto-revert-mode)
;; Automatically update buffers when contents change on disk:1 ends here

;; [[file:README.org::*Initial frame size for GUI][Initial frame size for GUI:1]]
(setq renz/frame-default-alist
      '(
        (tool-bar-lines . 0)
        (width . 180) ; chars
        (height . 60) ; lines
        (left . 125)
        (top . 125)))

(when (display-graphic-p)
  (setq initial-frame-alist renz/frame-default-alist)
  (setq default-frame-alist renz/frame-default-alist))
;; Initial frame size for GUI:1 ends here

;; [[file:README.org::*Marginalia][Marginalia:1]]
(use-package marginalia
  :ensure t
  :config (marginalia-mode))
;; Marginalia:1 ends here

;; [[file:README.org::*Highlight the line point is on][Highlight the line point is on:1]]
(global-hl-line-mode)
;; Highlight the line point is on:1 ends here

;; [[file:README.org::*Stop stupid bell][Stop stupid bell:1]]
;; Stop stupid bell
(setq ring-bell-function 'ignore)
;; Stop stupid bell:1 ends here

;; [[file:README.org::*Enable split-window dired copying][Enable split-window dired copying:1]]
(setq dired-dwim-target t)
;; Enable split-window dired copying:1 ends here

;; [[file:README.org::*Automatically create matching parens in programming modes][Automatically create matching parens in programming modes:1]]
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))
;; Automatically create matching parens in programming modes:1 ends here

;; [[file:README.org::*Delete whitespace on save][Delete whitespace on save:1]]
;; (add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Delete whitespace on save:1 ends here

;; [[file:README.org::*Don't wrap lines][Don't wrap lines:1]]
(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (toggle-truncate-lines nil))
;; Don't wrap lines:1 ends here

;; [[file:README.org::*Relative line numbers][Relative line numbers:1]]
(defun renz/display-line-numbers ()
  (setq display-line-numbers 'relative))
;; Relative line numbers:1 ends here

;; [[file:README.org::*Relative line numbers][Relative line numbers:2]]
(add-hook 'prog-mode-hook 'renz/display-line-numbers)
;; Relative line numbers:2 ends here

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
;; Enable colors in *compilation* buffer: https://stackoverflow.com/a/3072831/13215205
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

;; [[file:README.org::*Ignore risky .dir-locals.el][Ignore risky .dir-locals.el:1]]
(advice-add 'risky-local-variable-p :override #'ignore)
;; Ignore risky .dir-locals.el:1 ends here

;; [[file:README.org::*Prefer =rg= over =grep=][Prefer =rg= over =grep=:1]]
(when (executable-find "rg")
  (setq grep-program "rg"))
;; Prefer =rg= over =grep=:1 ends here

;; [[file:README.org::*Make ~dired~ human-readable][Make ~dired~ human-readable:1]]
(setq dired-listing-switches "-alFh")
(setq-default dired-hide-details-mode t)
;; Make ~dired~ human-readable:1 ends here

;; [[file:README.org::*Confirm when exiting Emacs][Confirm when exiting Emacs:1]]
(setq confirm-kill-emacs 'yes-or-no-p)
;; Confirm when exiting Emacs:1 ends here

;; [[file:README.org::*Prefer ~aspell~ over ~ispell~][Prefer ~aspell~ over ~ispell~:1]]
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"))
;; Prefer ~aspell~ over ~ispell~:1 ends here

;; [[file:README.org::*Smooth scrolling][Smooth scrolling:1]]
(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))
;; Smooth scrolling:1 ends here

;; [[file:README.org::*Backup and auto-save files][Backup and auto-save files:1]]
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups/"))
      ;; auto-save-file-name-transforms
      ;; '(("." ,temporary-file-directory t))
      )
;; Backup and auto-save files:1 ends here

;; [[file:README.org::*Code syntax in Markdown][Code syntax in Markdown:1]]
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;; Code syntax in Markdown:1 ends here

;; [[file:README.org::*Esup][Esup:1]]
(setq esup-depth 0)
;; Esup:1 ends here

;; [[file:README.org::*Reloading Emacs][Reloading Emacs:1]]
(use-package restart-emacs
  :ensure t)
;; Reloading Emacs:1 ends here

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

;; [[file:README.org::*Autocompletion][Autocompletion:1]]
(setq completion-styles '(flex basic partial-completion emacs22))
;; Autocompletion:1 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:2]]
(use-package orderless
  :ensure t
  :config
  (add-to-list 'completion-styles 'orderless)
  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; Autocompletion:2 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:3]]
(setq completions-format 'one-column)
;; Autocompletion:3 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:4]]
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
;; Autocompletion:4 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:5]]
(define-key completion-in-region-mode-map (kbd "C-n") 'renz/jump-completion)
(define-key completion-list-mode-map (kbd "C-n") 'next-completion)
(define-key completion-list-mode-map (kbd "C-p") 'previous-completion)
;; Autocompletion:5 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:6]]
(define-key completion-in-region-mode-map (kbd "C-j") 'renz/completion-accept)
(define-key completion-list-mode-map (kbd "C-j") 'choose-completion)
;; Autocompletion:6 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:7]]
(vertico-mode)
;; Autocompletion:7 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:8]]
(setq tab-always-indent 'complete)
;; Autocompletion:8 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:9]]
(unless (display-graphic-p)
  (corfu-terminal-mode +1))


(setq corfu-auto t
      corfu-auto-delay 0.0
      corfu-quit-no-match 'separator)

(global-corfu-mode)
;; Autocompletion:9 ends here

;; [[file:README.org::*Autocompletion][Autocompletion:10]]
(defun renz/disable-corfu-remote ()
  (when (and (fboundp 'corfu-mode)
             (file-remote-p default-directory))
    (corfu-mode -1)))
;; Autocompletion:10 ends here

;; [[file:README.org::*Org-mode][Org-mode:1]]
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
;; Org-mode:1 ends here

;; [[file:README.org::*Org-mode][Org-mode:3]]
(setq org-image-actual-width nil)
;; Org-mode:3 ends here

;; [[file:README.org::*Org-mode][Org-mode:5]]
(use-package ox-hugo
  :ensure t)
;; Org-mode:5 ends here

;; [[file:README.org::*Org-mode][Org-mode:6]]
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
;; Org-mode:6 ends here

;; [[file:README.org::*Org-mode][Org-mode:7]]
(use-package org
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
   ;; Outside the typical TODO/DONE states, I like to use DEAD as an indicator
   ;; that something is fully blocked, but not done.
   (setq org-todo-keywords '((sequence "TODO" "DEAD" "DONE")))
   (setq org-agenda-files '("~/.emacs.d/org/work.org")
         org-hugo-front-matter-format "yaml"))
;; Org-mode:7 ends here

;; [[file:README.org::*Org-mode][Org-mode:8]]
(use-package ob-async
  :ensure t
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            #'(lambda ()
                (require 'ob-sql-mode)
                (require 'hive2)))
  ;; Python has its own =:async yes= header argument we can use, so there's no
  ;; need to include it with ~ob-async~.
  (setq ob-async-no-async-languages-alist '("python"))
  ;; I'm having trouble rembering why I added this following line, except that I
  ;; belive it has something to do with exporting to HTML with syntax
  ;; highlighting.
  (setq org-html-htmlize-output-type 'css))
;; Org-mode:8 ends here

;; [[file:README.org::*=org-modern=][=org-modern=:1]]
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

(global-org-modern-mode)
;; =org-modern=:1 ends here

;; [[file:README.org::*Code block syntax highlighting for HTML export][Code block syntax highlighting for HTML export:1]]
(use-package htmlize
  :ensure t
  :after (org))
;; Code block syntax highlighting for HTML export:1 ends here

;; [[file:README.org::*Copying images out of org-babel][Copying images out of org-babel:1]]
(use-package ox-clip
  :ensure t)
;; Copying images out of org-babel:1 ends here

;; [[file:README.org::*SQL][SQL:1]]
(defun renz/sql-mode-hook ()
  (setq tab-width 4)
  (setq sqlformat-command 'sql-formatter)
  (setq sqlind-basic-offset 4))

(use-package sql-indent
  :ensure t
  :init
  (defvar renz/sql-indentation-offsets-alist
           '((select-clause 0)
             (insert-clause 0)
             (delete-clause 0)
             (update-clause 0)
             ,@sqlind-default-indentation-offsets-alist))

  ;; (defun renz/sql-indentation-offsets ()
  ;;   (add-hook 'sqlind-minor-mode-hook
  ;;             (setq sqlind-indentation-offsets-alist
  ;;                   renz/sql-indentation-offsets-alist)))

  :hook (sqlind-minor-mode . renz/sql-indentation-offsets))

(use-package sqlup-mode
  :ensure t
  :hook sql-interactive-mode
  :after (sql))

(use-package sql
  :hook  ((sql-mode . renz/sql-mode-hook)
          (sql-mode . sqlup-mode)
          (sql-mode . sqlind-minor-mode))
  :after (sqlup-mode sql)
  ;; :mode "\\.hql"
  )

;; (add-hook 'sql-mode-hook #'renz/sql-mode-hook)
(add-to-list 'auto-mode-alist '("\\.hql" . sql-mode))
;; (add-hook 'sql-mode-hook 'sqlup-mode)
;; (add-hook 'sql-mode-hook 'sqlind-minor-mode)
;; (add-hook 'sql-interactive-mode-hook 'sqlup-mode)

(use-package hive2
  :load-path "packages/"
  :after (sql))

(use-package ob-sql-mode
  :ensure t
  :after (sql))
;; SQL:1 ends here

;; [[file:README.org::*Python][Python:1]]
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))
;; Python:1 ends here

;; [[file:README.org::*Python][Python:2]]
(with-eval-after-load 'python
  (if (executable-find "mypy")
      (setq python-check-command "mypy"))
  (if (executable-find "pyright")
      (setq python-check-command "pyright")))
;; Python:2 ends here

;; [[file:README.org::*Python][Python:6]]
(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
;; Python:6 ends here

;; [[file:README.org::*Microsoft Windows][Microsoft Windows:1]]
(when (eq system-type 'windows-nt)
  ;; Set a better font on Windows
  (set-face-attribute 'default nil :font "Hack NF-12")
  ;; Alternate ispell when we've got msys on Windows
  (setq ispell-program-name "c:/msys64/usr/bin/aspell.exe")
  ;; Set default shell to pwsh
  (setq explicit-shell-file-name "pwsh")
  ;; Enable use of Winkey as super
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key
  ;; If we want to use a hotkey, we have to also register each
  ;; combination specifically, like this:
  (w32-register-hot-key [s-a])
  (w32-register-hot-key [s-b])
  (w32-register-hot-key [s-c])
  (w32-register-hot-key [s-d])
  (w32-register-hot-key [s-e])
  (w32-register-hot-key [s-f])
  (w32-register-hot-key [s-g])
  (w32-register-hot-key [s-h])
  (w32-register-hot-key [s-i])
  (w32-register-hot-key [s-j])
  (w32-register-hot-key [s-k])
  ;; s-l can NEVER be registered as a key combination, since Windows
  ;; handles it at a much lower level.
  ;; (w32-register-hot-key [s-l])
  (w32-register-hot-key [s-m])
  (w32-register-hot-key [s-n])
  (w32-register-hot-key [s-o])
  (w32-register-hot-key [s-p])
  (w32-register-hot-key [s-q])
  (w32-register-hot-key [s-r])
  (w32-register-hot-key [s-s])
  (w32-register-hot-key [s-t])
  (w32-register-hot-key [s-u])
  (w32-register-hot-key [s-v])
  (w32-register-hot-key [s-w])
  (w32-register-hot-key [s-x])
  (w32-register-hot-key [s-y])
  (w32-register-hot-key [s-z]))
;; Microsoft Windows:1 ends here

;; [[file:README.org::*macOS][macOS:1]]
(when (eq system-type 'darwin)
  ;; Uncomment this if we can't install Hack Nerd font
  ;; (set-face-attribute 'default nil :font "Menlo-14")
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-13")
  (exec-path-from-shell-initialize)
  (use-package vterm :ensure t))
;; macOS:1 ends here

;; [[file:README.org::*Linux][Linux:1]]
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-11")
  (use-package vterm :ensure t))
;; Linux:1 ends here

;; [[file:README.org::*Tramp][Tramp:1]]
(setq vc-handled-backends '(Git))
(setq remote-file-name-inhibit-locks t)
(setq tramp-inline-compress-start-size 1000)
(setq tramp-copy-size-limit 10000)
(setq tramp-verbose 1)
;; Tramp:1 ends here

;; [[file:README.org::*Tramp][Tramp:2]]
(setq tramp-use-ssh-controlmaster-options nil)
;; Tramp:2 ends here

;; [[file:README.org::*Tramp][Tramp:4]]
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path "~/.local/bin")
  (add-to-list 'tramp-remote-path "~/.conda/envs/robbmann/bin")
  ;; (remove-hook 'find-file-hook 'vc-find-file-hook)
  )
;; Tramp:4 ends here

;; [[file:README.org::*TreeSitter][TreeSitter:1]]
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after (tree-sitter))
;; TreeSitter:1 ends here

(provide 'init.el)
;;; init.el ends here
