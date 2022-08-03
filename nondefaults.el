;; Distraction-free writing (usually for my blog)
(require 'writeroom-mode)

;; Enable richer annotations using the Marginalia package
(marginalia-mode 1)

;; Use nov.el when opening an epub file
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; When running as a daemon or on macOS, ensure PATH is set correctly
(when (or (memq window-system '(mac ns x))
          (daemonp))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))


;; Autocomplete / Intellisense
;; ============================================================================
;; (use-package orderless
;;   :config
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

;; From the documentation: https://github.com/minad/corfu#installation-and-configuration
;; (use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  ;; :init
  ;; (global-corfu-mode))



;; Tree-sitter
;; ============================================================================
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)



;; Language Server (LSP) Specs
;; ============================================================================
(require 'lsp-mode)
(setq lsp-auto-guess-root t)
(setq lsp-log-io t)
(setq lsp-restart 'ignore) ; 'interactive auto-restart ignore
(setq lsp-enable-symbol-highlighting t)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-eldoc-hook nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-semantic-tokens-enable t)
(setq lsp-enable-folding nil)
(setq lsp-enable-imenu nil) ; when t, works with lsp-ui-imenu, consult-imenu, etc .. just not super useful in my workflow
(setq lsp-enable-snippet t)
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setq lsp-idle-delay 0.5)
(setq lsp-enable-file-watchers nil) ;; disable watching files all over the workspace - see https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
(setq lsp-enable-suggest-server-download nil) ;; don't offer to download servers all the time

(add-hook 'python-mode (lambda () (require 'lsp-pyright) (lsp)))
(add-hook 'go-mode 'lsp)
(add-hook 'rust-mode 'lsp)

(when (executable-find "python3")
  (setq lsp-pyright-python-executable-cmd "python3"))



;; LSP tramp remotes
;; ============================================================================
;; need this to enable my user paths, like ~/go/bin
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; From a very helpful reddit comment by /u/FrozenOnPluto:
;; https://www.reddit.com/r/emacs/comments/vhihjl/comment/igs6v68/?utm_source=share&utm_medium=web2x&context=3
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tramp-connection (lambda () (cons "pyright-langserver" lsp-pyright-langserver-command-args)))
  :major-modes '(python-mode)
  :remote? t
  :server-id 'pyright-remote
  :multi-root t
  :priority 3
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "pyright")
                                               (lsp-configuration-section "python")))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (ht-merge (lsp-configuration-section "pyright")
                                 (lsp-configuration-section "python")))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'pyright callback error-callback))
  :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                 ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                 ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))

;; This is based on the default example: https://emacs-lsp.github.io/lsp-mode/page/remote/
(require 'go-mode)
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tramp-connection "gopls")
  :major-modes '(go-mode)
  :remote? t
  :server-id 'gopls-remote))

;; I would love it if eglot works but I just can't get it to connect easily
;; (use-package eglot
;;   :ensure t)
