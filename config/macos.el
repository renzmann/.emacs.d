;; ============================================================================
;;                              macOS
;; ============================================================================
;; Default font
(when (eq system-type 'darwin)
  ;; Uncomment this if we can't install Hack Nerd font
  ;; (set-face-attribute 'default nil :font "Menlo-14")
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-13")
  (exec-path-from-shell-initialize)

  ;; Better terminal emulation
  (require 'vterm)
  (add-hook 'vterm-mode-hook 'turn-off-evil-mode)
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  )
