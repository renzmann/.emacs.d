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
;; Some parts of the theme are also modified in ~/.emacs.d/custom.el
;; TODO set theme based on time of day: https://stackoverflow.com/a/14760833/13215205
;; (use-package modus-themes
;;   :init (load-theme 'modus-vivendi)
;;   :bind ("<f5>" . 'modus-themes-toggle))
(use-package doom-themes)
(global-hl-line-mode)
;; Themes considered -- set using customize
;; doom-dark+
;; doom-dracula
;; doom-gruvbox
;; doom-material-dark
;; doom-material
;; doom-miarmare
;; doom-monokai-machine
;; doom-monokai-pro
;; doom-monokai-spectrum
;; doom-nord-aurora
;; doom-one
;; doom-palenight
;; doom-sourcerer
;; doom-spacegrey
;; doom-tomorrow-night
;; doom-vibrant
;; doom-xcode
;;
;; When I find one I want to stick with I'll remove the doom-themes
;; package to avoid bloat and just add in the theme I like under ~/.emacs.d/themes/

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

;; Visualize whitespace in programming buffers
(add-hook 'prog-mode-hook (whitespace-mode 1))

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
(add-hook 'prog-mode-hook (setq show-trailing-whitespace t))
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

;; Scroll the compilation window as text appears
(setq compilation-scroll-output t)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Enable the powerful 'orderless' completion style: https://github.com/oantolin/orderless
;; (use-package orderless
;;   :demand
;;   :custom
;;   (completion-styles '(orderless default))
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles . (partial-completion)))
;;                                    (eglot (styles . (orderless flex))))))

;; Built-in "fuzzy" completion style:
(setq completion-styles '(flex basic))

;; Enable fuzzy matching in minibuffer
;; (require 'icomplete)
;; (icomplete-mode 1)
;; (setq completion-styles '(flex basic))
;; (setq icomplete-scroll t)
;; (setq icomplete-show-matches-on-no-input t)
;; (setq icomplete-hide-common-prefix nil)
;; (setq icomplete-in-buffer t)
;; (unless (version< emacs-version "28.1")
;;   (setq icomplete-vertical-mode t))

(if (version< emacs-version "27.1")
    (progn
      (setq ido-enable-flex-matching t)
      (setq ido-everywhere t)
      (ido-mode 1))
  (fido-mode))

(unless (version< emacs-version "28.1")
  (fido-vertical-mode))

;; I tried both of the built-in options above and I can't stand the
;; popup delay. Vertico is still much faster, and works on older
;; versions of emacs, too.  I also don't feel like configuring the
;; keymaps ala C-j --> RET and C-M-i --> TAB
;; (use-package vertico
;;   :init (vertico-mode))

;; Diable tool bar
(tool-bar-mode -1)

;; Distraction-free writing (usually for my blog)
(use-package writeroom-mode)

;; Enable richer annotations using the Marginalia package
(marginalia-mode 1)

;; Use nov.el when opening an epub file
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; When running as a daemon or on macOS, ensure PATH is set correctly
(when (or (memq window-system '(mac ns x))
          (daemonp))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; When in an error-checking mode, bind helpful up/down commands
(use-package flymake
  :bind (("M-n" . #'flymake-goto-next-error)
         ("M-p" . #'flymake-goto-prev-error)
         ("C-c d" . #'flymake-show-buffer-diagnostics)))

;; Allow for custom resize of images when displaying in org mode
(setq org-image-actual-width nil)


;; Autocomplete / Intellisense
;; ============================================================================
;; From the documentation: https://github.com/minad/corfu#installation-and-configuration
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
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
;; I would love it if eglot just worked but I just can't get it to connect easily to pyright over tramp
(use-package eglot
  :hook (python-mode . eglot-ensure))

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
;; 	 . lsp-deferred)
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


;; Example error from pyright
;; --------------------------
;; /home/robb/tmp/errors.py/
;;   /home/robb/tmp/errors.py:1:1 - error: "foo" is not defined (reportUndefinedVariable)
;;   /home/robb/tmp/errors.py:1:1 - warning: Expression value is unused (reportUnusedExpression)
;;   /home/robb/tmp/errors.py:4:12 - error: Operator "+" not supported for types "str" and "Literal[1]"
;;     Operator "+" not supported for types "str" and "Literal[1]" (reportGeneralTypeIssues)
;; 2 errors, 1 warning, 0 informations
(add-to-list 'compilation-error-regexp-alist-alist
             ;; It would be nice if we could also capture the \\(error\\|warning\\) part as "KIND", but I got messed up on it
             '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'pyright)


;; Keymaps
;; ============================================================================
;; In a separate file so we have a quick key combo to jump to it
(load "~/.emacs.d/keymaps.el")
