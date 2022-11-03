;; ============================================================================
;;                      Misc. Editor Settings
;; ============================================================================
;; Fill-column for visual lines
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq fill-column 95)

;; Scroll bar
(scroll-bar-mode -1)

;; Emojis inline 👍
;; (add-hook 'after-init-hook #'global-emojify-mode)

;; Don't really need the splash screen
(setq inhibit-splash-screen t)

;; Automatically visit symlink sources
(setq find-file-visit-truename t)

;; Indent with spaces, not tabs by default
(setq-default indent-tabs-mode nil)

;; For files containing color escape codes, this provides a way to
;; render the colors in-buffer
(defun renz/display-ansi-colors ()
  (interactive)
  (require 'ansi-color)
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

;; TODO line numbers mess up fringe for org-modern.  Any way to
;; combine both?
(add-hook 'prog-mode-hook 'renz/display-line-numbers)
;; (add-hook 'text-mode-hook 'renz/display-line-numbers)

;; Delete the region when we yank on top of it
(delete-selection-mode t)

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; Scroll the *compilation* window as text appears
(setq compilation-scroll-output t)

;; Enable colors in *compilation* buffer: https://stackoverflow.com/a/3072831/13215205
(defun renz/colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'renz/colorize-compilation-buffer)

;; Disable tool bar
(tool-bar-mode -1)
;; The MENU bar, on the other hand (menu-bar-mode), is very handy, and
;; I don't think I'll ever disable it

;; Show laptop battery in the modeline
(display-battery-mode t)

;; Disable asking about risky variables from .dir-locals.el
;; https://emacs.stackexchange.com/a/44604
(advice-add 'risky-local-variable-p :override #'ignore)

;; Faster grep
(when (executable-find "rg")
  (setq grep-program "rg"))

;; Work around a bug where esup tries to step into the byte-compiled
;; version of `cl-lib', and fails horribly:
;; https://github.com/jschaf/esup/issues/85
(setq esup-depth 0)

;; Make dired human-readable
(setq dired-listing-switches "-alFh")

;; Don't quit emacs by accident
(setq confirm-kill-emacs 'yes-or-no-p)

;; If aspell is installed, use it instead of ispell
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"))

;; Smooth as butter scrolling
(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

;; Enable minimap if on graphical display
;; (when (display-graphic-p)
;;   (minimap-mode t))

;; Keep all backup files in one place
;; (let ((backup-dir (concat user-emacs-directory "backups")))
;;   (make-directory backup-dir t)
;;   (setq backup-directory-alist '(("." . backup-dir))))
(setq backup-directory-alist nil)

;; Enable syntax highlighting within code fences for markdown
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
