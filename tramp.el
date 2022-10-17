;; ============================================================================
;;                              TRAMP
;; ============================================================================
(add-to-list 'tramp-remote-path "~/.local/bin")
(setq vc-handled-backends '(git))
(setq tramp-verbose 1)

;; Disabling vc seems to get a little speed up
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
;; (setq vc-ignore-dir-regexp
;;       (format "\\(%s\\)\\|\\(%s\\)"
;;               vc-ignore-dir-regexp
;;               tramp-file-name-regexp))

;; TODO look into these - https://github.com/doomemacs/doomemacs/issues/3909
;; (setq tramp-inline-compress-start-size 1000)
;; (setq tramp-copy-size-limit 10000)
;; (setq vc-handled-backends '(Git))
;; (setq tramp-verbose 1)
;; (setq tramp-default-method "scp")
;; (setq tramp-use-ssh-controlmaster-options nil)
;; (setq projectile--mode-line "Projectile")
;; (setq tramp-verbose 1)

;; I often need to set these in ~/.ssh/config for TRAMP to speed up
;; Host *
;;      ControlMaster auto
;;      ControlPath ~/.ssh/master-%h:%p
;;      ControlPersist 10m
;;      ForwardAgent yes
;;      ServerAliveInterval 60

;; (require 'conda)
;; (setq conda-env-current-dir "/ssh:edgenode:~/.conda/envs/rae")
;; (setq conda-tramp-path (replace-regexp-in-string ".*:" ""
;;                                            (format "%s/bin" conda-env-current-dir)))
;; (add-to-list 'tramp-remote-path conda-tramp-path)
