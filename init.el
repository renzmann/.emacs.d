;;; Robert Enzmann's Emacs configuration

;; ============================================================================
;;                             Packages
;; ============================================================================
;; Custom at the top for ensuring packages are installed
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes '(default))
 '(eldoc-echo-area-use-multiline-p nil)
 '(evil-undo-system 'undo-redo)
 '(minimap-highlight-line nil)
 '(minimap-minimum-width 18)
 '(minimap-mode t)
 '(minimap-width-fraction 0.0)
 '(minimap-window-location 'right)
 '(mode-line-in-non-selected-windows t)
 '(org-agenda-files '("~/.emacs.d/org/work.org"))
 '(org-hugo-front-matter-format "yaml")
 '(package-selected-packages
   '(diff-hl ox-hugo minimap tramp restart-emacs gnuplot corfu-terminal corfu esup consult org-roam ob-async sqlup-mode sql-indent ripgrep blacken sqlformat pythonic f s reformatter change-inner vterm magit vertico tree-sitter-langs tree-sitter orderless ob-sql-mode yaml-mode exec-path-from-shell vimrc-mode csv-mode haskell-mode julia-mode lua-mode go-mode scala-mode rust-mode ef-themes markdown-mode eglot marginalia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:extend t :background "gray22"))))
 '(minimap-font-face ((t (:height 17 :family "DejaVu Sans Mono")))))

;; Keep packages in sync - only refreshing/installing if something is missing
(package-autoremove)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (and (cl-notevery 'package-installed-p package-selected-packages) (y-or-n-p "Update packages? "))
  (package-refresh-contents)
  (package-install-selected-packages))

(add-to-list 'load-path (concat user-emacs-directory "packages"))


;; ============================================================================
;;                         My configuration files
;; ============================================================================
;; Jump to any of these (with completion) using `M-x renz/jump-configuration`,
;; bound by default to `C-c s s`
(load-file (concat user-emacs-directory "config/keybindings.el"))
(load-file (concat user-emacs-directory "config/theme.el"))
(load-file (concat user-emacs-directory "config/misc.el"))
(load-file (concat user-emacs-directory "config/autocompletion.el"))
(load-file (concat user-emacs-directory "config/org.el"))
(load-file (concat user-emacs-directory "config/sql.el"))
(load-file (concat user-emacs-directory "config/python.el"))
(load-file (concat user-emacs-directory "config/windows.el"))
(load-file (concat user-emacs-directory "config/macos.el"))
(load-file (concat user-emacs-directory "config/linux.el"))
(load-file (concat user-emacs-directory "config/tramp.el"))
(load-file (concat user-emacs-directory "config/lsp.el"))
(load-file (concat user-emacs-directory "config/treesitter.el"))
(load-file (concat user-emacs-directory "config/projects.el"))
(load-file (concat user-emacs-directory "config/meow.el"))


(server-start)
