;; ============================================================================
;;                              Linux
;; ============================================================================
(when (eq system-type 'gnu/linux)

  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-11")
  (exec-path-from-shell-initialize))
