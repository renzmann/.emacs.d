;; ============================================================================
;;                          Autocompletion
;; ============================================================================
;; "flex" is the built-in "fuzzy" completion style
(setq completion-styles '(flex basic partial-completion emacs22))
(if (not (package-installed-p 'orderless))
    (add-to-list 'completion-styles 'flex)
  (require 'orderless)
  (add-to-list 'completion-styles 'orderless)
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; Fuzzy, live minibuffer completion
(vertico-mode)
;; Vertico works better for C-x C-f /ssh:<thing> than the built-in icomplete

;; Use TAB in place of C-M-i for completion-at-point
;; REMINDME: I think this causes some issues in shell-mode and the
;; like?  Can't remember...
;; (setq tab-always-indent 'complete)

;; Display candidates in *Completion* buffer vertically as a single list
(setq completions-format 'one-column)

;; Shortcuts for common completion actions
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
  "Close the *Completion* buffer without switching to it"
  (interactive)
  (kill-buffer "*Completions*"))

;; The combination of these two allows me to slam C-n several times to
;; quickly go down the candidate list
(define-key completion-in-region-mode-map (kbd "C-n") 'renz/jump-completion)
(define-key completion-list-mode-map (kbd "C-n") 'next-completion)
(define-key completion-list-mode-map (kbd "C-p") 'previous-completion)

;; REMINDME Don't use RET, TAB, and similar because there's a good
;; chance you'll mess up required functionality in shell, minibuffer,
;; and related modes

;; Accept the first result in the completion buffer without switching
(define-key completion-in-region-mode-map (kbd "C-j") 'renz/completion-accept)
(define-key completion-list-mode-map (kbd "C-j") 'choose-completion)

;; ----------------------------------------------------------------------------
;;                              company-mode
;; ----------------------------------------------------------------------------
;; Mostly obsolete, since I don't use company anymore, but kept for
;; historical reasons.

;; ;; Company causes TRAMP to hang if we look for commands too fast
;; (defun renz/disable-company-remote-shell ()
;;   (when (and (fboundp 'company-mode)
;;              (file-remote-p default-directory))
;;     (company-mode -1)))

;; (add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'shell-mode-hook 'renz/disable-company-remote-shell)
;; (setq company-minimum-prefix-length 2)
;; (setq company-idle-delay
;;       (lambda () (if (company-in-string-or-comment) nil 0.0)))
;; (setq company-tooltip-align-annotations t)
;; (setq company-tooltip-flip-when-above t)
;; (setq company-tooltip-margin 2)

;; ----------------------------------------------------------------------------
;;                                 corfu
;; ----------------------------------------------------------------------------
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(defun renz/disable-corfu-remote ()
  (when (and (fboundp 'corfu-mode)
             (file-remote-p default-directory))
    (corfu-mode -1)))

;; (add-hook 'python-mode-hook 'renz/disable-corfu-remote)

(setq corfu-auto t
      corfu-auto-delay 0.0
      corfu-quit-no-match 'separator)

(global-corfu-mode)
