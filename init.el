;;; Robert Enzmann's Emacs configuration
;;
;; This will work in any canonical init file location, such as
;;  ~/.emacs, ~/.emacs.d/init.el, or ~/.config/emacs/init.el



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
   '(corfu vterm evil magit vertico tree-sitter-langs tree-sitter orderless ob-sql-mode sql-indent yaml-mode exec-path-from-shell vimrc-mode csv-mode haskell-mode julia-mode lua-mode go-mode scala-mode rust-mode ef-themes markdown-mode eglot pyvenv marginalia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Keep packages in sync - only refreshing/installing if something is missing
(package-initialize)
(package-autoremove)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (cl-notevery 'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))

(add-to-list 'load-path "~/.emacs.d/packages")



;; ============================================================================
;;                           Color Theme
;; ============================================================================
;; Prot's themes have been reliably legible in nearly every situation.
(setq ef-themes-headings ; read the manual's entry of the doc string
      '((0 . (1.9))
        (1 . (1.8))
        (2 . (1.7))
        (3 . (1.6))
        (4 . (1.5))
        (5 . (1.4)) ; absence of weight means `bold'
        (6 . (1.3))
        (7 . (1.2))
        (t . (1.1))))

(setq ef-themes-to-toggle '(ef-day ef-night))
(load-theme 'ef-night :no-confirm)




;; ============================================================================
;;                      Misc. Editor Settings
;; ============================================================================
;; Initial frame size for GUI
(setq renz/frame-default-alist
      '(
        (tool-bar-lines . 0)
        (width . 180) ; chars
        (height . 60) ; lines
        (left . 150)
        (top . 150)))
(when (display-graphic-p)
  (setq initial-frame-alist renz/frame-default-alist)
  (setq default-frame-alist renz/frame-default-alist))

;; Adds helpful information in the margin when using the minibuffer
(when (package-installed-p 'marginalia)
  (marginalia-mode))

;; Highlight the line point is on
(global-hl-line-mode)

;; Stop stupid bell
(setq ring-bell-function 'ignore)

;; Clock in statusline
(setq display-time-day-and-date t)
(display-time)

;; Enable split-window dired copying
(setq dired-dwim-target t)

;; Automatically create matching parens in programming modes
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; Show markers for trailing whitespace and delete on save
;; (add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't wrap lines
(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (toggle-truncate-lines nil))

;; Relative line numbers in programming and writing modes
(defun renz/display-line-numbers ()
  (setq display-line-numbers 'relative))
(add-hook 'prog-mode-hook 'renz/display-line-numbers)
(add-hook 'text-mode-hook 'renz/display-line-numbers)

;; Delete the region when we yank on top of it
(delete-selection-mode t)

;; Add a "File -> Open recent..." option to the menu
(recentf-mode t)

(defun renz/recentf-find-file ()
  "Find a recent file using the minibuffer with completion"
  (interactive)
  (unless (find-file (completing-read "Find recent file: " recentf-list))
    (message "Aborting...")))

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; Scroll the *compilation* window as text appears
(setq compilation-scroll-output t)

;; Enable colors in *compilation* buffer: https://stackoverflow.com/a/3072831/13215205
(require 'ansi-color)
(defun renz/colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'renz/colorize-compilation-buffer)

;; Disable tool bar
(tool-bar-mode -1)
;; The MENU bar, on the other hand (menu-bar-mode), is very handy, and
;; I don't think I'll ever disable it

;; Allow for custom resize of images when displaying in org mode
(setq org-image-actual-width nil)

;; Show laptop battery in the modeline
(display-battery-mode t)

;; Add ~/.local/bin to Eshell PATH when on *nix
;; FIXME is this really necessary?
(defun renz/eshell-local-bin ()
  "Ensure ~/.local/bin is on PATH when starting eshell"
  (unless (eq system-type 'windows-nt)
    (eshell/addpath "~/.local/bin")))

;; eshell/addpath is buffer-local, so we have to run this as a hook
(add-hook 'eshell-mode-hook 'renz/eshell-local-bin)

;; Enable .dir-locals.el for remote files
(setq enable-remote-dir-locals t)

;; Disable asking about risky variables from .dir-locals.el
;; https://emacs.stackexchange.com/a/44604
(advice-add 'risky-local-variable-p :override #'ignore)

;; Vim keybindings
(require 'evil)
;; (evil-mode 1)
(add-hook 'prog-mode-hook 'turn-on-evil-mode)
(add-hook 'text-mode-hook 'turn-on-evil-mode)

;; Faster grep
(when (executable-find "rg")
  (setq grep-program "rg"))



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
;; Vertico works better for C-x C-f /ssh:<thing>

;; (if (version< emacs-version "27.1")
;;     (progn
;;       (setq ido-enable-flex-matching t)
;;       (setq ido-everywhere t)
;;       (ido-mode 1))
;;   (fido-mode)
;;   ;; Have TAB complete using the first option and continue, instead of
;;   ;; popping up the *Completions* buffer
;;   (define-key icomplete-minibuffer-map [remap minibuffer-complete] 'icomplete-force-complete))

;; On newer versions of emacs, set minibuffer completion candidates to
;; display vertically
;; (unless (version< emacs-version "28.1")
;;   ;; Sometimes I had to customize the icomplete-compute-delay variable
;;   ;; to 0.0 to avoid delay on M-x popup
;;   (setq icomplete-compute-delay 0.0)
;;   (fido-vertical-mode))

;; Use TAB in place of C-M-i for completion-at-point
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

;; FIXME how do we get rid of the double-TAB behavior on examples like this?
;; (completioreg)
;; This first completes out to (completion-reg), requiring another TAB
;; for the *completion* buffer to show up Docs don't explain a way
;; around it.

;; Enable auto completion and configure quitting
(global-corfu-mode)
(setq corfu-auto t
      corfu-quit-no-match 'separator)


;; ============================================================================
;;                             Org mode
;; ============================================================================
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t )
   (python . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)

;; A kill-block command for working with src blocks
(defun renz/org-kill-src-block ()
  "Kill the src block around point, if applicable."
  (interactive)
  (org-babel-remove-result)
  (org-mark-element)
  (kill-region nil nil t))

;; https://willschenk.com/articles/2019/using_org_mode_in_hugo/
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

;; Enable asynchronous execution of src blocks
(when (package-installed-p 'ob-async)
  (require 'ob-async)
  (add-hook 'ob-async-pre-execute-src-block-hook
            '(lambda ()
	       (require 'ob-sql-mode)
	       (require 'hive2))))

(setq ob-async-no-async-languages-alist '("python"))
(setq org-html-htmlize-output-type 'css)



;; ============================================================================
;;                                SQL
;; ============================================================================
(require 'hive2)
(require 'ob-sql-mode)
(add-to-list 'auto-mode-alist '("\\.hql" . sql-mode))



;; ============================================================================
;;                              Python
;; ============================================================================
;; Example error from pyright
;; --------------------------
;; /home/robb/tmp/errors.py/
;;   /home/robb/tmp/errors.py:1:1 - error: "foo" is not defined (reportUndefinedVariable)
;;   /home/robb/tmp/errors.py:1:1 - warning: Expression value is unused (reportUnusedExpression)
;;   /home/robb/tmp/errors.py:4:12 - error: Operator "+" not supported for types "str" and "Literal[1]"
;;     Operator "+" not supported for types "str" and "Literal[1]" (reportGeneralTypeIssues)
;; 2 errors, 1 warning, 0 informations
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             ;; It would be nice if we could also capture the
             ;; \\(error\\|warning\\) part as "KIND", but I got messed
             ;; up on it
             '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'pyright)

;; Extra check commands for C-c C-v
(if (executable-find "mypy")
    (setq python-check-command "mypy"))
(if (executable-find "pyright")
    (setq python-check-command "pyright"))

;; I ran into something similar to this on Windows:
;; https://github.com/jorgenschaefer/elpy/issues/733
;;
;; The culprit was "App Execution Aliases" with python and python3
;; redirecting to the windows store. Using:
;;
;;     winkey -> Manage app execution aliases -> uncheck python and python3
;;
;; fixed it.

;; Also on Windows - a `pip install` of `pyreadline3' is required to
;; make tab-completion work at all. It provides the `readline' import
;; symbol.

;; Virtualenvs - require .dir-locals.el to have e.g.:
;;   ((python-mode . ((python-shell-virtualenv-root . "/path/to/my/.venv"))))
;; However, this only operates on `run-python' shells.
;;
;; `pyvenv' solves the otherwise very annoying problem of getting
;; external tools like `compile' and `eshell' to also use our virtual
;; environment's python.  I may still use .dir-locals.el to set things
;; like the python-check-command on a per-project basis, though.
(when (package-installed-p 'pyvenv)
  (pyvenv-mode)
  ;; (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
  (pyvenv-tracking-mode)
  ;; (setenv "WORKON_HOME" "~/.conda/envs")
  )

;; Enable semantic mode for more intelligent code parsing
;; https://www.gnu.org/software/emacs/manual/html_node/semantic/Semantic-mode.html
;; (add-hook 'python-mode-hook 'semantic-mode)

;; Don't mark the check command and virtualenv variables as unsafe
(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)



;; ============================================================================
;;                        Microsoft Windows
;; ============================================================================
(when (eq system-type 'windows-nt)
  ;; Set a better font on Windows
  (set-face-attribute 'default nil :font "Hack NF-12")
  ;; Alternate ispell when we've got msys on Windows
  (setq ispell-program-name "c:/msys64/usr/bin/aspell.exe")
  ;; Set default shell to pwsh
  (setq explicit-shell-file-name "pwsh")
  ;; Enable use of Winkey as super
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key
  ;; If we want to use a hotkey, we have to also register each
  ;; combination specifically, like this:
  ;;
  ;; (w32-register-hot-key [s-p])
  ;;
  ;; s-l can NEVER be registered as a key combination, since Windows
  ;; handles it at a much lower level.
  )



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



;; ============================================================================
;;                              Linux
;; ============================================================================
(when (eq system-type 'gnu/linux)
  (exec-path-from-shell-initialize))



;; ============================================================================
;;                              TRAMP
;; ============================================================================
(add-to-list 'tramp-remote-path "~/.local/bin")
(add-to-list 'tramp-remote-path "~/.conda/envs/rae/bin")

;; TODO look into these - https://github.com/doomemacs/doomemacs/issues/3909
;; (setq tramp-inline-compress-start-size 1000)
;; (setq tramp-copy-size-limit 10000)
;; (setq vc-handled-backends '(Git))
;; (setq tramp-verbose 1)
;; (setq tramp-default-method "scp")
;; (setq tramp-use-ssh-controlmaster-options nil)
;; (setq projectile--mode-line "Projectile")
;; (setq tramp-verbose 1)



;; ============================================================================
;; 			      TreeSitter
;; ============================================================================
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)



;; ============================================================================
;;                           Keybindings
;; ============================================================================
;; Keymap settings that don't belong under any of the previous headers
;; ----------------------------------------
;; Expanded defaults
;; ----------------------------------------
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-z") 'zap-up-to-char)

;; A better version of `dabbrev'
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Better buffer list for C-x C-b
(global-set-key [remap list-buffers] 'ibuffer)

;; When flycheck is running (usually from a language server), bind next/previous
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Reserved for users:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html
;; ----------------------------------------
;; C-c <letter> bindings
;; ----------------------------------------
(global-set-key (kbd "C-c a") #'org-agenda)
;; (global-set-key (kbd "C-c b") ')
(global-set-key (kbd "C-c c") #'org-capture)
;; (global-set-key (kbd "C-c d") ')
;; (global-set-key (kbd "C-c e") ')
;; (global-set-key (kbd "C-c f") ')
;; (global-set-key (kbd "C-c g") ')
;; (global-set-key (kbd "C-c h") ')
(global-set-key (kbd "C-c i") #'imenu)
(global-set-key (kbd "C-c j") #'imenu)  ; matches major modes that use C-c C-j
;; (global-set-key (kbd "C-c k") ')
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c m") #'ef-themes-toggle)
;; (global-set-key (kbd "C-c n") ')
;; (global-set-key (kbd "C-c o") ')
;; (global-set-key (kbd "C-c p") ')
;; (global-set-key (kbd "C-c q") ')
(global-set-key (kbd "C-c r") #'renz/recentf-find-file)
;; (global-set-key (kbd "C-c s") ')
(global-set-key (kbd "C-c t") #'org-babel-detangle)
;; (global-set-key (kbd "C-c u") ')
;; (global-set-key (kbd "C-c v") ')
(global-set-key (kbd "C-c w") #'renz/org-kill-src-block)
;; (global-set-key (kbd "C-c x") ')
;; (global-set-key (kbd "C-c y") ')
;; (global-set-key (kbd "C-c z") ')
;; ----------------------------------------
;; F5 - F9
;; ----------------------------------------
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "M-<f5>") 'recompile)
;; (global-set-key (kbd "<f6>") ')
;; (global-set-key (kbd "M-<f6>") ')
;; (global-set-key (kbd "<f7>") ')
;; (global-set-key (kbd "M-<f7>") ')
;; (global-set-key (kbd "<f8>") ')
;; (global-set-key (kbd "M-<f8>") ')
;; (global-set-key (kbd "<f9>") ')
;; (global-set-key (kbd "M-<f9>") ')


;; ============================================================================
;;                              Daemon
;; ============================================================================
(server-start)
