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
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Redirect custom so it doesn't edit this file
(setq custom-file "~/.emacs.d/custom.el")

;; Load the custom file, which sets the 'package-selected-packages variable for package-install-selected-packages
(when (file-exists-p custom-file)
  (load custom-file))

;; Add new packages interactively with either M-x package-install or M-x list-packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents))

(package-autoremove)
(package-install-selected-packages)
;; Remove packages by:
;; 1. Remove the entry from package-selected-packages via M-x package-delete or M-x list-packages
;; 2. Remove any configuration in this file
;; 3. Either restart emacs or load-file ~/.emacs.d/custom.el
;; 4. Use (package-autoremove)

;; There's a lot of complexity around initializing packages: https://ianyepan.github.io/posts/setting-up-use-package/
;; So, it's either:
;;   1. Hack the package management myself, with potential pitfalls from not understanding startup process, but probably
;;      learn along the way
;;   2. Use use-package, even the the whole 'macro' thing doesn't make sense yet

(eval-when-compile
  (require 'use-package))


;; Editor Settings
;; ============================================================================
(when (version< emacs-version "28.1")
  (package-install 'modus-themes))

(use-package emacs
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-region '(bg-only no-extend))
  :config
  ;; Load the theme of your choice:
  (load-theme 'modus-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; Highlight line that point is on
(global-hl-line-mode)

;; Set a pretty Nerd Font
;; Test char and monospace:
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; -> => <=  ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
;; TODO loop this over possible font names
;; TODO good excercise for learning `let` block
(if (eq system-type 'windows-nt)
    (setq my-font "Hack NF")
  (setq my-font "Hack Nerd Font Mono"))

(if (eq system-type "darwin")
    (setq my-font-size "-15")
  (setq my-font-size "-12"))

(when (member my-font (font-family-list))
  (set-face-attribute 'default nil :font (concat my-font my-font-size)))

;; On Windows, with Msys, use aspell instead of ispell for spellchecking
(when (eq system-type 'windows-nt)
  (setq ispell-program-name "c:/msys64/usr/bin/aspell.exe"))

;; TODO: On Windows, set default shell to powershell (pwsh)
;;
;;

;; Stop stupid bell
(setq ring-bell-function 'ignore)

;; Clock in statusline
(setq display-time-day-and-date t)
(display-time)

;; Enable split-window dired copying
(setq dired-dwim-target t)

;; Enable EDE mode: https://www.gnu.org/software/emacs/manual/html_node/emacs/EDE.html
;; (global-ede-mode t)

;; Line and number modes
;; (when (version<= "26.0.50" emacs-version)
;;   (global-display-line-numbers-mode))
;; (setq display-line-numbers-type 'relative)
;; (setq column-number-mode t)

;; Automatically create matching parens in programming modes
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Show markers for trailing whitespace and delete on save
(add-hook 'prog-mode-hook 'whitespace-mode)
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

;; TAB cycle if there are only few candidates
;; (setq completion-cycle-threshold 3)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; The powerful 'orderless' completion style: https://github.com/oantolin/orderless
;; Trying to figure out how how to get this only for M-x in icomplete
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Built-in "fuzzy" completion style:
;; (setq completion-styles '(flex basic partial-completion))

;; (if (version< emacs-version "27.1")
;;     (progn
;;       (setq ido-enable-flex-matching t)
;;       (setq ido-everywhere t)
;;       (ido-mode 1))
;;   (fido-mode)
;;   ;; Have TAB complete using the first option and continue, instead of
;;   ;; popping up the *Completions* buffer
;;   (define-key icomplete-minibuffer-map [remap minibuffer-complete] 'icomplete-force-complete))

;; (unless (version< emacs-version "28.1")
;;   ;; I had to customize the icomplete-compute-delay variable to 0.0 to avoid delay on M-x popup
;;   (fido-vertical-mode))

;; Before setting the built-in support above, I was using =vertico=,
;; but this had an occasional issue where the minibuffer would stall
;; after messing up some text, instead of automatically resetting
(use-package vertico
  :init
  (vertico-mode))

;; Diable tool bar
(tool-bar-mode -1)

;; Enable richer minibuffer annotations using the Marginalia package
(marginalia-mode 1)

;; Use nov.el when opening an epub file
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; When running as a daemon or on macOS, ensure PATH is set correctly
(when (or (memq window-system '(mac ns x))
          (daemonp))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Allow for custom resize of images when displaying in org mode
(setq org-image-actual-width nil)


;; Flymake - compiler output parsing and diagnostics management
;; ============================================================================
(use-package flymake
  :bind
  (("M-n" . #'flymake-goto-next-error)
   ("M-p" . #'flymake-goto-prev-error)
   ("C-c d" . #'flymake-show-buffer-diagnostics))
  :config
  (setq flymake-wrap-around t))


;; Autocomplete / Intellisense
;; ============================================================================
;; From the documentation: https://github.com/minad/corfu#installation-and-configuration
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; **Enabling auto-completion breaks eshell for me - it's auto inserting tabs/spaces**
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

;; Add extensions -- grabbed this especially for inline filepath completion
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-symbol)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p i" . cape-ispell)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)


;; Tree-sitter
;; ============================================================================
(use-package tree-sitter
  :init (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (use-package tree-sitter-hl)
  (use-package tree-sitter-langs)
  (use-package tree-sitter-debug)
  (use-package tree-sitter-query))


;; Language Server (LSP) Specs
;; ============================================================================
;; Currently trying this out - I dislike the attitude of the new lsp-mode author: https://github.com/joaotavora/eglot/issues/180#issuecomment-446293381
;; (use-package eglot
;;   :hook (python-mode . eglot-ensure))

;; From a very helpful reddit comment by /u/FrozenOnPluto about geting LSP set up on remote systems:
;; https://www.reddit.com/r/emacs/comments/vhihjl/comment/igs6v68/?utm_source=share&utm_medium=web2x&context=3
;; (use-package lsp-mode
;;   :hook ((c-mode          ; clangd
;;           c++-mode        ; clangd
;;           c-or-c++-mode   ; clangd
;;           java-mode       ; eclipse-jdtls
;;           js-mode         ; ts-ls (tsserver wrapper)
;;           js-jsx-mode     ; ts-ls (tsserver wrapper)
;;           typescript-mode ; ts-ls (tsserver wrapper)
;;           python-mode     ; pyright
;;           web-mode        ; ts-ls/HTML/CSS
;;           )
;;           . lsp-deferred)
;;   :commands lsp
;;   :config
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-log-io t)
;;   (setq lsp-restart 'ignore) ; 'interactive auto-restart ignore
;;   (setq lsp-enable-symbol-highlighting t)
;;   (setq lsp-enable-on-type-formatting nil)
;;   (setq lsp-signature-auto-activate nil)
;;   (setq lsp-signature-render-documentation nil)
;;   (setq lsp-eldoc-hook nil)
;;   (setq lsp-modeline-code-actions-enable nil)
;;   (setq lsp-modeline-diagnostics-enable nil)
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-semantic-tokens-enable t)
;;   (setq lsp-enable-folding nil)
;;   (setq lsp-enable-imenu t) ; when t, works with lsp-ui-imenu, consult-imenu, etc
;;   (setq lsp-enable-snippet t)
;;   (setq read-process-output-max (* 1024 1024)) ;; 1MB
;;   (setq lsp-idle-delay 0.5)
;;   (setq lsp-enable-file-watchers nil) ;; disable watching files all over the workspace - see https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
;;   (setq lsp-enable-suggest-server-download nil) ;; don't offer to download servers all the time
;;   )

;; (use-package lsp-ui
;;   :config
;;   (setq lsp-ui-sideline-show-diagnostics t))

;; (use-package lsp-pyright
;;   :after lsp-mode
;;   :hook (python-mode . (lambda () (require 'lsp-pyright)))
;;   :init
;;   (when (executable-find "python3")
;;     (setq lsp-pyright-python-executable-cmd "python3"))
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-tramp-connection (lambda () (cons "pyright-langserver" lsp-pyright-langserver-command-args)))
;;     :major-modes '(python-mode)
;;     :remote? t
;;     :server-id 'pyright-remote
;;     :multi-root t
;;     :priority 3
;;     :initialization-options (lambda () (ht-merge (lsp-configuration-section "pyright")
;;                                                  (lsp-configuration-section "python")))
;;     :initialized-fn (lambda (workspace)
;;                       (with-lsp-workspace workspace
;;                         (lsp--set-configuration
;;                          (ht-merge (lsp-configuration-section "pyright")
;;                                    (lsp-configuration-section "python")))))
;;     :download-server-fn (lambda (_client callback error-callback _update?)
;;                           (lsp-package-ensure 'pyright callback error-callback))
;;     :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
;;                                    ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
;;                                    ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))



;; LSP tramp remotes
;; ============================================================================
;; need this to enable my user paths, like ~/go/bin
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;; Go (golang.org)
;; ============================================================================
;; This is based on the default example: https://emacs-lsp.github.io/lsp-mode/page/remote/
;; (use-package go-mode
;;   :hook lsp
;;   :config
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-tramp-connection "gopls")
;;     :major-modes '(go-mode)
;;     :remote? t
;;     :server-id 'gopls-remote)))


;; Rust (rust-lang.org)
;; ============================================================================
;; (use-package rust-mode
;;   :hook (rust-mode . lsp))


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
(add-to-list 'compilation-error-regexp-alist-alist
             ;; It would be nice if we could also capture the
             ;; \\(error\\|warning\\) part as "KIND", but I got messed
             ;; up on it
             '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'pyright)

;; Set the best python interpreter (run-python) for this system
(if (eq system-type 'windows-nt)
    (if (executable-find "ipython")
        ;; Ipython is *usualy* a superior REPL
        (setq python-shell-interpreter "ipython"
              python-shell-interpreter-args "-i")
      ;; Windows redirects 'python' and 'python3' to the Microsoft store,
      ;; sometimes...
      (setq python-shell-interpreter "py")))

;; If we have pyright, use it as the default command for checking files
(if (executable-find "pyright")
    (setq python-check-command "pyright"))

(add-hook 'python-mode-hook 'blacken-mode)


;; Org-mode
;; ============================================================================
;; A kill-block command for working with org-mode src blocks
(defun renz/org-kill-src-block ()
  "Kill the src block around point, if applicable."
  (interactive)
  (org-babel-remove-result)
  (org-mark-element)
  (kill-region nil nil t))

(use-package org
  :bind ("C-c w" . #'renz/org-kill-src-block))


;; Keymaps
;; ============================================================================
;; In a separate file so we have a quick key combo to jump to it
(load "~/.emacs.d/keymaps.el")
