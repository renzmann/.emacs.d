;; ============================================================================
;;                      Misc. Editor Settings
;; ============================================================================
;; Don't really need the splash screen
(setq inhibit-splash-screen t)

;; Automatically visit symlink sources
(setq find-file-visit-truename t)

;; Indent with spaces, not tabs by default
(setq-default indent-tabs-mode nil)

;; For files containing color escape codes, this provides a way to
;; render the colors in-buffer
(require 'ansi-color)
(defun renz/display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; Enable horizontal scrolling with mouse
;; https://stackoverflow.com/a/67758169
(setq mouse-wheel-tilt-scroll t)

;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(unless (version< emacs-version "27.1")
  (setq switch-to-buffer-obey-display-actions t))

;; Enable up/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Automatically update buffers when contents change on disk
(global-auto-revert-mode)

;; Initial frame size for GUI
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

;; Adds helpful information in the margin when using the minibuffer
(when (package-installed-p 'marginalia)
  (marginalia-mode))

;; Highlight the line point is on
(global-hl-line-mode)

;; Stop stupid bell
(setq ring-bell-function 'ignore)

;; Clock in statusline
(setq display-time-day-and-date t)
(display-time)

;; Enable split-window dired copying
(setq dired-dwim-target t)

;; Automatically create matching parens in programming modes
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Show markers for trailing whitespace and delete on save
;; (add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't wrap lines
(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (toggle-truncate-lines nil))

;; Relative line numbers in programming and writing modes
(defun renz/display-line-numbers ()
  (setq display-line-numbers 'relative))
(add-hook 'prog-mode-hook 'renz/display-line-numbers)
(add-hook 'text-mode-hook 'renz/display-line-numbers)

;; Delete the region when we yank on top of it
(delete-selection-mode t)

;; Add a "File -> Open recent..." option to the menu
(recentf-mode t)

(defun renz/recentf-find-file ()
  "Find a recent file using the minibuffer with completion"
  (interactive)
  (unless (find-file (completing-read "Find recent file: " recentf-list))
    (message "Aborting...")))

(defun renz/find-tag ()
  "Use completing-read to navigate to a tag"
  (interactive)
  (xref-find-definitions (completing-read "Find tag: " tags-completion-table)))

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; Scroll the *compilation* window as text appears
(setq compilation-scroll-output t)

;; Enable colors in *compilation* buffer: https://stackoverflow.com/a/3072831/13215205
(require 'ansi-color)
(defun renz/colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'renz/colorize-compilation-buffer)

;; Disable tool bar
(tool-bar-mode -1)
;; The MENU bar, on the other hand (menu-bar-mode), is very handy, and
;; I don't think I'll ever disable it

;; Allow for custom resize of images when displaying in org mode
(setq org-image-actual-width nil)

;; Show laptop battery in the modeline
(display-battery-mode t)

;; Add ~/.local/bin to Eshell PATH when on *nix
;; FIXME is this really necessary?
;; (defun renz/eshell-local-bin ()
;;   "Ensure ~/.local/bin is on PATH when starting eshell"
;;   (unless (eq system-type 'windows-nt)
;;     (eshell/addpath "~/.local/bin")))

;; eshell/addpath is buffer-local, so we have to run this as a hook
;; (add-hook 'eshell-mode-hook 'renz/eshell-local-bin)

;; Enable .dir-locals.el for remote files
(setq enable-remote-dir-locals t)

;; Disable asking about risky variables from .dir-locals.el
;; https://emacs.stackexchange.com/a/44604
(advice-add 'risky-local-variable-p :override #'ignore)

;; Vim keybindings
;; (require 'evil)
;; (evil-mode 1)
;; (add-hook 'prog-mode-hook 'turn-on-evil-mode)
;; (add-hook 'text-mode-hook 'turn-on-evil-mode)

;; Faster grep
(when (executable-find "rg")
  (setq grep-program "rg"))
