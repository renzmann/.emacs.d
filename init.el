;; Robert Enzmann's emacs configuration
;; Either in ~/.emacs or ~/.emacs.d/init.el
(load-theme 'wombat)

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

;; Delete the region when we yank on top of it
(delete-selection-mode t)

;; Add a "File -> Open recent..." option to the menu
(recentf-mode t)

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; Scroll the *compilation* window as text appears
(setq compilation-scroll-output t)

;; Enable colors in *compilation* buffer: https://stackoverflow.com/a/3072831/13215205
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; Use TAB in place of C-M-i for competion-at-point
(setq tab-always-indent 'complete)

;; Built-in "fuzzy" completion style:
(setq completion-styles '(flex basic partial-completion))

;; Fuzzy, live minibuffer completion
(if (version< emacs-version "27.1")
    (progn
      (setq ido-enable-flex-matching t)
      (setq ido-everywhere t)
      (ido-mode 1))
  (fido-mode)
  ;; Have TAB complete using the first option and continue, instead of
  ;; popping up the *Completions* buffer
  (define-key icomplete-minibuffer-map [remap minibuffer-complete] 'icomplete-force-complete))

;; On newer versions of emacs, set minibuffer completion candidates to
;; display vertically
(unless (version< emacs-version "28.1")
  ;; Sometimes I had to customize the icomplete-compute-delay variable
  ;; to 0.0 to avoid delay on M-x popup
  (fido-vertical-mode))

;; Diable tool bar
(tool-bar-mode -1)

;; Allow for custom resize of images when displaying in org mode
(setq org-image-actual-width nil)

;; Show laptop battery in the modeline
(display-battery-mode t)

;; Org mode
;; ============================================================================
(setq org-babel-load-languages '((emacs-lisp . t) (python . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)

;; Python
;; ============================================================================
;; Example error from pyright
;; --------------------------
;; /home/robb/tmp/errors.py/
;;   /home/robb/tmp/errors.py:1:1 - error: "foo" is not defined (reportUndefinedVariable)
;;   /home/robb/tmp/errors.py:1:1 - warning: Expression value is unused (reportUnusedExpression)
;;   /home/robb/tmp/errors.py:4:12 - error: Operator "+" not supported for types "str" and "Literal[1]"
;;     Operator "+" not supported for types "str" and "Literal[1]" (reportGeneralTypeIssues)
;; 2 errors, 1 warning, 0 informations
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             ;; It would be nice if we could also capture the
             ;; \\(error\\|warning\\) part as "KIND", but I got messed
             ;; up on it
             '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'pyright)

;; Keybindings
;; ============================================================================
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-z") 'zap-up-to-char)

;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Better buffer list for C-x C-b
(global-set-key [remap list-buffers] 'ibuffer)

;; Global Org bindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c r") #'recentf)
(global-set-key (kbd "C-c t") #'org-babel-detangle)
