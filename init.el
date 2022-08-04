;;; init.el --- my emacs init
;;
;; Author : Robert Enzmann
;;
;;; Commentary:
;; I put this here to make the linter stop whining.
;;
;;;* Code


;; External Packages
;; ============================================================================
;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Redirect custom so it doesn't edit this file
(setq custom-file "~/.emacs.d/custom.el")

;; Load the custom file, which sets the 'package-selected-packages variable for package-install-selected-packages
(when (file-exists-p custom-file)
  (load custom-file))

;; Add new packages interactively with either M-x package-install or M-x list-packages
(package-refresh-contents)
(package-install-selected-packages)
;; Remove packages by:
;; 1. Remove the entry from package-selected-packages via M-x customize-variable RET package-selected-packages
;; 2. Remove any configuration in this file
;; 3. Either restart emacs or load-file ~/.emacs.d/custom.el
;; 4. Use (package-autoremove)


;; Editor Settings
;; ============================================================================
;; Some parts of the theme are also modified in ~/.emacs.d/custom.el
(load-theme 'tango-dark)

;; Test char and monospace:
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; -> =>     ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
;; TODO loop this over possible font names
;; TODO good excercise for learning `let` block
(if (eq system-type 'windows-nt)
    (setq my-font "Hack NF")
  (setq my-font "Hack Nerd Font Mono"))

(if (eq system-type "darwin")
    (setq my-font-size "-15")
  (setq my-font-size "-12"))

(when (member my-font (font-family-list))
  (set-face-attribute 'default nil :font (concat my-font my-font-size)))

;; On Windows, with Msys, use aspell instead of ispell for spellchecking
(when (eq system-type 'windows-nt)
  (setq ispell-program-name "c:/msys64/usr/bin/aspell.exe"))

;; TODO: On Windows, set default shell to powershell (pwsh)
;;
;;

;; Stop stupid bell
(setq ring-bell-function 'ignore)

;; Clock in statusline
(setq display-time-day-and-date t)
(display-time)

;; Enable split-window dired copying
(setq dired-dwim-target t)

;; Line and number modes
;; (when (version<= "26.0.50" emacs-version)
;;   (global-display-line-numbers-mode))
;; (setq display-line-numbers-type 'relative)
;; (setq column-number-mode t)

;; Automatically create matching parens in lisp mode
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Show markers for trailing whitespace and delete on save
(add-hook 'prog-mode-hook (setq show-trailing-whitespace t))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't wrap lines
(setq-default truncate-lines t)

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Delete the region when we yank on top of it
(delete-selection-mode t)

;; Find recent files
(require 'recentf)
(recentf-mode t)

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; Scroll the compilation window as text appears
(setq compilation-scroll-output t)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Enable fuzzy matching in minibuffer
(require 'icomplete)
(setq completion-styles '(flex basic))
(setq icomplete-scroll t)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-in-buffer t)
(unless (version< emacs-version "28.1")
  (setq icomplete-vertical-mode t))
(icomplete-mode 1)


;; Non-default configuration
;; ============================================================================
;; everything that doesn't ship with emacs configured in `nondefaults.el'
(load "~/.emacs.d/nondefaults.el")
(load "~/.emacs.d/keymaps.el")
