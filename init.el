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
 '(custom-safe-themes '(default))
 '(eldoc-echo-area-use-multiline-p nil)
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(esup consult doom-themes org-roam ob-async ob-ipython company sqlup-mode minimap sql-indent ripgrep projectile blacken lsp-mode sqlformat pythonic f s reformatter change-inner expand-region vterm evil magit vertico tree-sitter-langs tree-sitter orderless ob-sql-mode yaml-mode exec-path-from-shell vimrc-mode csv-mode haskell-mode julia-mode lua-mode go-mode scala-mode rust-mode ef-themes markdown-mode eglot marginalia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:extend t :background "gray22")))))

;; REMINDME: is this necessary? We get a slight startup time hit by including it.
;; (package-initialize)

;; Keep packages in sync - only refreshing/installing if something is missing
(package-autoremove)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (and (cl-notevery 'package-installed-p package-selected-packages) (y-or-n-p "Update packages? "))
  (package-refresh-contents)
  (package-install-selected-packages))

(add-to-list 'load-path "~/.emacs.d/packages")


;; ============================================================================
;;                         My configuration files
;; ============================================================================
;; Jump to any of these (with completion) using `M-x renz/jump-configuration`,
;; bound by default to `C-c s s`
(load-file "~/.emacs.d/theme.el")
(load-file "~/.emacs.d/misc.el")
(load-file "~/.emacs.d/autocompletion.el")
(load-file "~/.emacs.d/org.el")
(load-file "~/.emacs.d/sql.el")
(load-file "~/.emacs.d/python.el")
(load-file "~/.emacs.d/windows.el")
(load-file "~/.emacs.d/macos.el")
(load-file "~/.emacs.d/linux.el")
(load-file "~/.emacs.d/tramp.el")
(load-file "~/.emacs.d/lsp.el")
(load-file "~/.emacs.d/treesitter.el")
(load-file "~/.emacs.d/projects.el")
(load-file "~/.emacs.d/minimap.el")
(load-file "~/.emacs.d/keybindings.el")


(server-start)
