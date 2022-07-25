;;; init.el --- my emacs init
;;
;; Author : Robert Enzmann
;;
;;; Commentary:
;; I put this here to make the linter stop whining.
;;
;;; Code

;; External Packages
;; ============================================================================
;; Enable MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
              (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "Your version of Emacs does not support SSL connections."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/"))))

(setq package-archive-priorities '(("gnu" . 30)("melpa-stable" . 20)("melpa" . 10)))

;; Add new packages interactively with either M-x package-install, or by adding it via `M-x customize-variable RET package-selected-packages`
(package-install-selected-packages)
;; Remove packages by:
;; 1. Remove the entry from package-selected-packages via M-x customize-variable
;; 2. Either restart emacs or load-file ~/.emacs.d/custom.el
;; 3. Use (package-autoremove)

;; Editor Settings
;; ============================================================================
;; Some parts of the theme are also modified in ~/.emacs.d/custom.el
(load-theme 'wombat)

;; Stop stupid bell
(setq ring-bell-function 'ignore)

;; Clock in statusline
(display-time)

;; Enable split-window dired copying
(setq dired-dwim-target t)

;; Interactive completion (bundled with emacs)
;; Very helpful resource on it: https://www.masteringemacs.org/article/introduction-to-ido-mode
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
  ;; `ffap` find file at point - we can try this later but "some people hate it"
  ;; (setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
  ;; modify 'completion-ignored-extensions with regexes to ignore some things (maybe useful for backup and object files?)
  ;; (setq ido-ignore-extensions t)
(ido-mode t)

;; Line and number modes
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))
(setq display-line-numbers-type 'relative)
(setq column-number-mode t)

;; Automatically create matching parens in lisp mode
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Show markers for trailing whitespace and delete on save
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't wrap lines
(setq-default truncate-lines t)

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Delete the region when we yank on top of it
(delete-selection-mode t)

;; Make cursor a vertical bar
;; (setq-default cursor-type 'bar)

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

;; redirect custom so it doesn't edit this file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))


;; Keybindings
;; ============================================================================
;; Reserved for users: C-c <letter>
(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-c r") 'comment-or-uncomment-region)  ;; can we make the same keybinding as above work based on whether we've highlighted a region?

;; Reserved for users: f5 - f8
(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "S-<f5>") 'find-file-at-point)
(global-set-key (kbd "<f6>") 'find-function-at-point)
(global-set-key (kbd "S-<f6>") 'find-symbol-at-point)
