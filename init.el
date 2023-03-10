;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

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
  (package-install-selected-packages)
  (require 'use-package))
;; Packages:1 ends here

;; [[file:README.org::*Packages][Packages:2]]
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
;; Packages:2 ends here

;; [[file:README.org::*Microsoft Windows][Microsoft Windows:1]]
(when (memq system-type '(windows-nt cygwin ms-dos))
  ;; Set a better font on Windows
  (set-face-attribute 'default nil :font "Hack NF-12")
  ;; Alternate ispell when we've got msys on Windows
  (setq ispell-program-name "aspell.exe")
  ;; Set default shell to pwsh
  ;; (setq explicit-shell-file-name "pwsh")
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
  (exec-path-from-shell-initialize))
;; macOS:1 ends here

;; [[file:README.org::*Linux][Linux:1]]
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-11"))
;; Linux:1 ends here

;; [[file:README.org::*Theme: ~ef-themes~][Theme: ~ef-themes~:1]]
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
;; Theme: ~ef-themes~:1 ends here

;; [[file:README.org::*~dabbrev~: swap ~M-/~ and ~C-M-/~][~dabbrev~: swap ~M-/~ and ~C-M-/~:1]]
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
;; ~dabbrev~: swap ~M-/~ and ~C-M-/~:1 ends here

;; [[file:README.org::*Mode line][Mode line:2]]
(setq column-number-mode t
      mode-line-in-non-selected-windows t)
;; Mode line:2 ends here

;; [[file:README.org::*=eldoc=][=eldoc=:1]]
(setq eldoc-echo-area-use-multiline-p nil)
;; =eldoc=:1 ends here

;; [[file:README.org::*Remember minibuffer history][Remember minibuffer history:1]]
(setq history-length 25)
(savehist-mode 1)
;; Remember minibuffer history:1 ends here

;; [[file:README.org::*Colored output in ~eshell~][Colored output in ~eshell~:1]]
(add-hook 'eshell-preoutput-filter-functions  'ansi-color-apply)
;; Colored output in ~eshell~:1 ends here

;; [[file:README.org::*Recent files menu][Recent files menu:1]]
(recentf-mode t)
;; Recent files menu:1 ends here

;; [[file:README.org::*Fill-column][Fill-column:1]]
(setq-default fill-column 80)
;; Fill-column:1 ends here

;; [[file:README.org::*Scroll bar][Scroll bar:1]]
(scroll-bar-mode -1)
;; Scroll bar:1 ends here

;; [[file:README.org::*Inhibit splash screen][Inhibit splash screen:1]]
(setq inhibit-splash-screen t)
;; Inhibit splash screen:1 ends here

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

;; [[file:README.org::*Automatically update buffers when contents change on disk][Automatically update buffers when contents change on disk:1]]
(global-auto-revert-mode)
;; Automatically update buffers when contents change on disk:1 ends here

;; [[file:README.org::*Highlight the line point is on][Highlight the line point is on:1]]
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Delete whitespace on save:1 ends here

;; [[file:README.org::*Don't wrap lines][Don't wrap lines:1]]
(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (toggle-truncate-lines nil))
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

;; [[file:README.org::*Ignore risky .dir-locals.el][Ignore risky .dir-locals.el:1]]
(advice-add 'risky-local-variable-p :override #'ignore)
;; Ignore risky .dir-locals.el:1 ends here

;; [[file:README.org::*Prefer =rg= and =fd= over =grep= and =find=][Prefer =rg= and =fd= over =grep= and =find=:1]]
(when (executable-find "rg")
  (setq grep-program "rg"))

(when (executable-find "fd")
  (setq find-program "fd"))
;; Prefer =rg= and =fd= over =grep= and =find=:1 ends here

;; [[file:README.org::*Make ~dired~ human-readable][Make ~dired~ human-readable:1]]
(setq dired-listing-switches "-alFh")
;; (setq-default dired-hide-details-mode t)
;; Make ~dired~ human-readable:1 ends here

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

;; [[file:README.org::*Keybound functions][Keybound functions:1]]
(defun renz/--jump-section (dirname prompt extension)
  "For internal use: prompt for a file under `dirname' in the user
emacs config site with matching `extension' regexp"
  (find-file
   (concat dirname
	   (completing-read prompt
			    (directory-files dirname nil extension)))))

(setq renz/site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))

(defun renz/jump-configuration ()
  "Prompt for a .el file in my site-lisp folder, then go there."
  (interactive)
  (renz/--jump-section renz/site-lisp-dir
		       "Elisp config files: "
		       ".*\.el$"))

(defun renz/jump-org ()
  "Prompt for an org file in my emacs directory, then go there."
  (interactive)
  (renz/--jump-section renz/org-home
		       "Org files: "
		       ".*\.org$"))

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

;; [[file:README.org::*=C-c b= scroll-bar-mode][=C-c b= scroll-bar-mode:1]]
;; (global-set-key (kbd "C-c a") #')
(global-set-key (kbd "C-c b") #'scroll-bar-mode)
;; =C-c b= scroll-bar-mode:1 ends here

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
(global-set-key (kbd "C-c i i") #'renz/jump-init)
(global-set-key (kbd "C-c i l") #'renz/jump-configuration)
;; =C-c i= jump to a header in my configuration:1 ends here

;; [[file:README.org::*=C-c k= kill all but one space][=C-c k= kill all but one space:1]]
(global-set-key (kbd "C-c k") #'just-one-space)
;; =C-c k= kill all but one space:1 ends here

;; [[file:README.org::*=C-c q= replace regexp][=C-c q= replace regexp:1]]
(global-set-key (kbd "C-c q") #'replace-regexp)
;; =C-c q= replace regexp:1 ends here

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

;; [[file:README.org::*=C-c w=][=C-c w=:1]]
(global-set-key (kbd "C-c w") #'whitespace-mode)
;; =C-c w=:1 ends here

;; [[file:README.org::*=C-c x=][=C-c x=:1]]
;; (global-set-key (kbd "C-c x") #')
;; =C-c x=:1 ends here

;; [[file:README.org::*=C-c z=][=C-c z=:1]]
;; (global-set-key (kbd "C-c z") #')
;; =C-c z=:1 ends here

;; [[file:README.org::*=C-c= Other bindings][=C-c= Other bindings:1]]
(global-set-key (kbd "C-c ;") #'comment-line)  ; TTY-friendly
(global-set-key (kbd "C-c <DEL>") #'backward-kill-sexp)  ;; TTY-frindly
(global-set-key (kbd "C-c <SPC>") #'mark-sexp)  ;; TTY-friendly
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

;; [[file:README.org::*Consult][Consult:1]]
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
;; Consult:1 ends here

;; [[file:README.org::*Completion style: Orderless][Completion style: Orderless:1]]
(setq completion-styles '(flex basic partial-completion emacs22))
;; Completion style: Orderless:1 ends here

;; [[file:README.org::*Completion style: Orderless][Completion style: Orderless:2]]
(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless)

  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; Completion style: Orderless:2 ends here

;; [[file:README.org::*Nicer display of ~*Completions*~][Nicer display of ~*Completions*~:1]]
(setq completions-format 'one-column)
;; Nicer display of ~*Completions*~:1 ends here

;; [[file:README.org::*Nicer display of ~*Completions*~][Nicer display of ~*Completions*~:2]]
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 20)
;; Nicer display of ~*Completions*~:2 ends here

;; [[file:README.org::*Keybindings to interact with ~*Completions*~][Keybindings to interact with ~*Completions*~:1]]
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
;; Keybindings to interact with ~*Completions*~:1 ends here

;; [[file:README.org::*Keybindings to interact with ~*Completions*~][Keybindings to interact with ~*Completions*~:2]]
(define-key completion-in-region-mode-map (kbd "C-n") 'renz/jump-completion)
(define-key completion-list-mode-map (kbd "C-n") 'next-completion)
(define-key completion-list-mode-map (kbd "C-p") 'previous-completion)
;; Keybindings to interact with ~*Completions*~:2 ends here

;; [[file:README.org::*Keybindings to interact with ~*Completions*~][Keybindings to interact with ~*Completions*~:3]]
(define-key completion-in-region-mode-map (kbd "C-j") 'renz/completion-accept)
(define-key completion-list-mode-map (kbd "C-j") 'choose-completion)
;; Keybindings to interact with ~*Completions*~:3 ends here

;; [[file:README.org::*Minibuffer completion with ~vertico~ and ~marginalia~][Minibuffer completion with ~vertico~ and ~marginalia~:1]]
(use-package vertico
  :config (vertico-mode))
;; Minibuffer completion with ~vertico~ and ~marginalia~:1 ends here

;; [[file:README.org::*Minibuffer completion with ~vertico~ and ~marginalia~][Minibuffer completion with ~vertico~ and ~marginalia~:2]]
(use-package marginalia
  :config (marginalia-mode))
;; Minibuffer completion with ~vertico~ and ~marginalia~:2 ends here

;; [[file:README.org::*Completion at point with ~corfu~][Completion at point with ~corfu~:1]]
(unless (display-graphic-p)
  (use-package corfu-terminal
    :config
    (corfu-terminal-mode +1)))

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

;; [[file:README.org::*Completion at point with ~corfu~][Completion at point with ~corfu~:3]]
(setq tab-always-indent 'complete)
;; Completion at point with ~corfu~:3 ends here

;; [[file:README.org::*Org-mode][Org-mode:1]]
(setq renz/org-home "~/org/")
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
;; Org-mode:1 ends here

;; [[file:README.org::*Org-mode][Org-mode:3]]
(setq org-image-actual-width nil)
;; Org-mode:3 ends here

;; [[file:README.org::*Org-mode][Org-mode:5]]
(use-package ox-hugo)
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
  ((org-mode . (lambda () (progn
                           (add-hook 'after-save-hook #'org-babel-tangle :append :local)
                           (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors))))
   )

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
   ;; Outside the typical TODO/DONE states, I like to use DEAD as an indicator
   ;; that something is fully blocked, but not done.
   (setq org-todo-keywords '((sequence "TODO" "DEAD" "DONE")))
   (setq org-agenda-files '("~/.emacs.d/org/work.org")
         org-hugo-front-matter-format "yaml"))
;; Org-mode:7 ends here

;; [[file:README.org::*Org-mode][Org-mode:8]]
(use-package ob-async

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
(use-package org-modern
  :after org
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
   org-ellipsis "???"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ????
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ??????????????? " "?????????????????????????????????????????????")
   org-agenda-current-time-string
   "??? now ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????")

  (global-org-modern-mode))
;; =org-modern=:1 ends here

;; [[file:README.org::*SQL][SQL:1]]
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

(add-hook 'sqlind-minor-mode-hook #'renz/sql-indentation-offsets)
(add-to-list 'auto-mode-alist '("\\.hql" . sql-mode))
(add-hook 'sql-mode-hook #'renz/sql-mode-hook)
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

(use-package hive2
  :after (sql))

(use-package ob-sql-mode
  :after (sql))
;; SQL:1 ends here

;; [[file:README.org::*sql-formatter][sql-formatter:1]]
(use-package sqlformat
  :after (sql))
;; sql-formatter:1 ends here

;; [[file:README.org::*Python][Python:1]]
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))
;; Python:1 ends here

;; [[file:README.org::*Python][Python:2]]
(use-package python
  :config
  (if (executable-find "mypy")
      (setq python-check-command "mypy"))
  (if (executable-find "pyright")
      (setq python-check-command "pyright"))
  (add-hook 'python-mode-hook #'blacken-mode))
;; Python:2 ends here

;; [[file:README.org::*Python][Python:6]]
(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
;; Python:6 ends here

;; [[file:README.org::*pyrightconfig.json, Tramp, and eglot][pyrightconfig.json, Tramp, and eglot:1]]
(use-package pyrightconfig
  :after (python))
;; pyrightconfig.json, Tramp, and eglot:1 ends here

;; [[file:README.org::*blacken][blacken:1]]
(use-package blacken
  :bind ("C-c p" . blacken-mode)
  :after (python))
;; blacken:1 ends here

;; [[file:README.org::*Markdown][Markdown:1]]
(defun renz/md-hook ()
  (visual-fill-column-mode)
  (setq-local fill-column 80))

(use-package markdown-mode
  ;; :config
  ;; (add-hook 'markdown-mode-hook #'renz/md-hook)
  )

(use-package poly-markdown
  :after (markdown-mode))
;; Markdown:1 ends here

;; [[file:README.org::*Code syntax in Markdown][Code syntax in Markdown:1]]
(use-package poly-mode
  :mode ("\\.md" . poly-markdown-mode))
;; Code syntax in Markdown:1 ends here

;; [[file:README.org::*yaml][yaml:1]]
(use-package yaml-mode)
;; yaml:1 ends here

;; [[file:README.org::*Haskell][Haskell:1]]
(use-package haskell-mode)
;; Haskell:1 ends here

;; [[file:README.org::*Golang][Golang:1]]
(use-package go-mode)
;; Golang:1 ends here

;; [[file:README.org::*Lua][Lua:1]]
(use-package lua-mode)
;; Lua:1 ends here

;; [[file:README.org::*Rust][Rust:1]]
(use-package rust-mode)
;; Rust:1 ends here

;; [[file:README.org::*Scala][Scala:1]]
(use-package scala-mode)
;; Scala:1 ends here

;; [[file:README.org::*AutoHotkey][AutoHotkey:1]]
(use-package ahk-mode
  :mode "\\.ahk\\'")
;; AutoHotkey:1 ends here

;; [[file:README.org::*csv-mode][csv-mode:1]]
(use-package csv-mode
  :mode "\\.csv\\'")
;; csv-mode:1 ends here

;; [[file:README.org::*Tramp][Tramp:1]]
(setq vc-handled-backends '(Git))
(setq file-name-inhibit-locks t)
(setq tramp-inline-compress-start-size 1000)
(setq tramp-copy-size-limit 10000)
(setq tramp-verbose 1)
;; Tramp:1 ends here

;; [[file:README.org::*Tramp][Tramp:2]]
(setq tramp-use-ssh-controlmaster-options nil)
;; Tramp:2 ends here

;; [[file:README.org::*Tramp][Tramp:4]]
(add-to-list 'tramp-remote-path "~/.local/bin")
(add-to-list 'tramp-remote-path "~/.conda/envs/robbmann/bin")
;; Tramp:4 ends here

;; [[file:README.org::*Visual fill column][Visual fill column:1]]
(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))
;; Visual fill column:1 ends here

;; [[file:README.org::*Magit][Magit:1]]
(use-package magit)
;; Magit:1 ends here

;; [[file:README.org::*Change or copy inner/outer][Change or copy inner/outer:1]]
(use-package change-inner
  :bind (("C-c c i" . change-inner)
         ("C-c c o" . change-outer)
         ("C-c y i" . copy-inner)
         ("C-c y o" . copy-outer)))
;; Change or copy inner/outer:1 ends here

;; [[file:README.org::*eww - search engine and browser][eww - search engine and browser:1]]
(use-package eww
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html/?q="))
;; eww - search engine and browser:1 ends here

;; [[file:README.org::*diff-hl][diff-hl:1]]
(use-package diff-hl
  :bind ("C-c v" . diff-hl-mode))
;; diff-hl:1 ends here

;; [[file:README.org::*GNU Plot][GNU Plot:1]]
(use-package gnuplot
  :after (org))
;; GNU Plot:1 ends here

;; [[file:README.org::*change-inner][change-inner:1]]
(use-package change-inner
  :bind (("C-c c i" . change-inner)
         ("C-c c o" . change-outer)
         ("C-c y i" . yank-inner)
         ("C-c y o" . yank-outer)))
;; change-inner:1 ends here

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

;; [[file:README.org::*Code block syntax highlighting for HTML export][Code block syntax highlighting for HTML export:1]]
(use-package htmlize
  :after (org))
;; Code block syntax highlighting for HTML export:1 ends here

;; [[file:README.org::*Copying images out of org-babel][Copying images out of org-babel:1]]
(use-package ox-clip
  :after org)
;; Copying images out of org-babel:1 ends here

;; [[file:README.org::*Start a server for =emacsclient=][Start a server for =emacsclient=:1]]
(server-start)
;; Start a server for =emacsclient=:1 ends here

(provide 'init.el)
;;; init.el ends here
