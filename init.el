;;; init.el --- my emacs init
;;
;; Author : Robert Enzmann
;;
;;; Commentary:
;; I put this here to make the linter stop whining.
;;
;;;* Code



;; External Packages
;; ============================================================================
;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Redirect custom so it doesn't edit this file
(setq custom-file "~/.emacs.d/custom.el")
;; Load the custom file, which sets the 'package-selected-packages variable for package-install-selected-packages
(when (file-exists-p custom-file)
  (load custom-file))

;; Add new packages interactively with either M-x package-install or M-x list-packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)
;; Remove packages by:
;; 1. Remove the entry from package-selected-packages via M-x customize-variable
;; 2. Either restart emacs or load-file ~/.emacs.d/custom.el
;; 3. Use (package-autoremove)
;; 4. Remove any configuration in this file
(require 'use-package)
(require 'writeroom-mode)



;; Editor Settings
;; ============================================================================
;; Some parts of the theme are also modified in ~/.emacs.d/custom.el
;; (load-theme 'wombat)
;; (load-theme 'nord)
(load-theme 'material)

;; Test char and monospace:
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
;; TODO loop this over possible font names
;; TODO good excercise for learning `let` block
(if (eq system-type 'windows-nt)
    (setq my-font "Hack NF")
  (setq my-font "Hack Nerd Font Mono"))

(if (eq system-type "darwin")
    (setq my-font-size "-12")
  (setq my-font-size "-15"))

(when (member my-font (font-family-list))
  (set-face-attribute 'default nil :font (concat my-font my-font-size)))

;; On Windows, with Msys, use aspell instead of ispell for spellchecking
(when (eq system-type 'windows-nt)
  (setq ispell-program-name "c:/msys64/usr/bin/aspell.exe"))

;; Stop stupid bell
(setq ring-bell-function 'ignore)

;; Clock in statusline
(setq display-time-day-and-date t)
(display-time)

;; Enable split-window dired copying
(setq dired-dwim-target t)

;; Line and number modes
;; (when (version<= "26.0.50" emacs-version)
;;   (global-display-line-numbers-mode))
;; (setq display-line-numbers-type 'relative)
;; (setq column-number-mode t)

;; Automatically create matching parens in lisp mode
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Show markers for trailing whitespace and delete on save
(add-hook 'prog-mode-hook (setq show-trailing-whitespace t))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't wrap lines
(setq-default truncate-lines t)

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Delete the region when we yank on top of it
(delete-selection-mode t)

;; Make cursor a vertical bar
;; (setq-default cursor-type 'bar)

;; Find recent files
(require 'recentf)
(recentf-mode t)

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; Scroll the compilation window as text appears
(setq compilation-scroll-output t)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; When running as a daemon or on macOS, ensure PATH is set correctly
(when (or (memq window-system '(mac ns x))
          (daemonp))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Use nov.el when opening an epub file
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package pyvenv
  :config
  (setq python-shell-completion-native-enable nil))

(use-package which-key
  :init
  (which-key-mode))



;; Autocomplete / Intellisense
;; ============================================================================
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; From the documentation: https://github.com/minad/corfu#installation-and-configuration
(use-package corfu
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
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))



;; Keybindings / keymaps
;; ============================================================================
;; Better defaults
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c d") 'flymake-show-buffer-diagnostics)

;; Reserved for users: C-c <letter>
(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-c p") (lambda () (interactive) (customize-variable 'package-selected-packages)))

;; Reserved for users: f5 - f8
(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "S-<f5>") 'find-file-at-point)
(global-set-key (kbd "<f6>") 'find-function-at-point)
(global-set-key (kbd "S-<f6>") 'find-symbol-at-point)
(global-set-key (kbd "<f8>") 'writeroom-mode)
(global-set-key [remap list-buffers] 'ibuffer)



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
(use-package lsp-mode
  :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          go-mode         ; gopls
          java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          rust-mode       ; rust-analyzer
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; pyright
          web-mode        ; ts-ls/HTML/CSS
          ) . lsp-deferred)
  :commands lsp
  :config
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
  )

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))



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
(use-package go-mode :ensure t)
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tramp-connection "gopls")
  :major-modes '(go-mode)
  :remote? t
  :server-id 'gopls-remote))

;; I would love it if eglot works but I just can't get it to connect easily
;; (use-package eglot
;;   :ensure t)
