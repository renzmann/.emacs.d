;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: internal
;; URL: https://robbmann.io/

;;; Commentary:
;; A mostly minimal, reproducible Emacs configuration.  This file is
;; automatically tangled from README.org, with header/footer comments on each
;; code block that allow for de-tangling the source back to README.org when
;; working on this file directly.

;;; Code:

;; [[file:README.org::*Custom][Custom:1]]
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))
;; Custom:1 ends here

;; [[file:README.org::*Proxy settings][Proxy settings:1]]
(defun renz/enable-proxy ()
  (interactive)
  "Turn on HTTP proxy."
  (let ((proxy-file (expand-file-name "proxy.el" user-emacs-directory)))
    (when (file-exists-p proxy-file)
      (load-file proxy-file)
      (setq url-proxy-services
            `(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . ,(concat renz/proxy-host ":" renz/proxy-port))
              ("https" . ,(concat renz/proxy-host ":" renz/proxy-port))))
      (setq url-http-proxy-basic-auth-storage
            (list
             (list
              (concat renz/proxy-host ":" renz/proxy-port)
              (cons renz/proxy-login
                    (base64-encode-string
                     (concat renz/proxy-login ":" (password-read "Proxy password: "))))))))))

(defun renz/disable-proxy ()
  (interactive)
  "Turn off HTTP proxy."
  (setq url-proxy-services nil)
  (setq url-http-proxy-basic-auth-storage nil))
;; Proxy settings:1 ends here

;; [[file:README.org::*Packages][Packages:1]]
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; Packages:1 ends here

;; [[file:README.org::*Packages][Packages:2]]
(defun renz/windowsp ()
  "Are we on Microsoft Windows?"
  (memq system-type '(windows-nt cygwin ms-dos)))
;; Packages:2 ends here

;; [[file:README.org::*Packages][Packages:3]]
(when-let* ((on-win (renz/windowsp))
            (has-uname (executable-find "uname"))
            (uname (shell-command-to-string "uname"))
            (is-msys (string-prefix-p "MSYS" uname))
            (package-dir-expandable (string-prefix-p "~" package-user-dir))
            (expand-package-dir (expand-file-name "gnupg" package-user-dir))
            (new-package-user-dir (replace-regexp-in-string "^\\([a-zA-Z]\\):/" "/\\1/" expand-package-dir)))
  (setq package-gnupghome-dir new-package-user-dir))
;; Packages:3 ends here

;; [[file:README.org::*Packages][Packages:4]]
(defun renz/package-sync ()
  "Remove unused sources and install any missing ones."
  (interactive)
  (package-autoremove)
  (package-install-selected-packages)
  (package-vc-install-selected-packages))
;; Packages:4 ends here

;; [[file:README.org::*Packages][Packages:5]]
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
;; Packages:5 ends here

;; [[file:README.org::*Microsoft Windows][Microsoft Windows:2]]
(defun renz/find-file (chosen-dir regex)
  (interactive "DSearch dir: \nsRegexp: ")
  (let ((chosen-file (completing-read "File: " (find-lisp-find-files chosen-dir regex))))
    (find-file chosen-file)))

(global-set-key (kbd "C-c f f") #'renz/find)
(global-set-key (kbd "C-c f d") #'find-lisp-find-dired)
;; Microsoft Windows:2 ends here

;; [[file:README.org::*macOS][macOS:1]]
(when (eq system-type 'darwin)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))
;; macOS:1 ends here

;; [[file:README.org::*Font][Font:1]]
(cond ((x-list-fonts "Hack Nerd Font")
       (add-to-list 'default-frame-alist '(font . "Hack Nerd Font-12")))
      ;; ((x-list-fonts "Segoe UI Emoji")
      ;;  (add-to-list 'default-frame-alist '(font . "Segoe UI Emoji-12")))
      )
;; Font:1 ends here

;; [[file:README.org::*Theme][Theme:1]]
(if (version< emacs-version "31.0")
    (load-theme 'modus-vivendi)
  (load-theme 'modus-vivendi-tinted t)
  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  (global-set-key (kbd "C-c m") #'modus-themes-toggle))
;; Theme:1 ends here

;; [[file:README.org::*Stop stupid bell][Stop stupid bell:1]]
;; Stop stupid bell
(setq ring-bell-function 'ignore)
;; Stop stupid bell:1 ends here

;; [[file:README.org::*Start a server for =emacsclient=][Start a server for =emacsclient=:1]]
(server-start)
;; Start a server for =emacsclient=:1 ends here

;; [[file:README.org::*Don't hang when visiting files with extremely long lines][Don't hang when visiting files with extremely long lines:1]]
(global-so-long-mode t)
;; Don't hang when visiting files with extremely long lines:1 ends here

;; [[file:README.org::*Unicode][Unicode:1]]
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; Unicode:1 ends here

;; [[file:README.org::*Mode line][Mode line:2]]
(setq column-number-mode t
      mode-line-in-non-selected-windows t)
;; Mode line:2 ends here

;; [[file:README.org::*Remember minibuffer history][Remember minibuffer history:1]]
(savehist-mode 1)
;; Remember minibuffer history:1 ends here

;; [[file:README.org::*Render ASCII color escape codes][Render ASCII color escape codes:1]]
(defun renz/display-ansi-colors ()
  "Render colors in a buffer that contains ASCII color escape codes."
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
;; Render ASCII color escape codes:1 ends here

;; [[file:README.org::*Color output in ~eshell~ and =*compilation*=][Color output in ~eshell~ and =*compilation*=:1]]
(add-hook 'compilation-filter-hook #'renz/display-ansi-colors)
;; Color output in ~eshell~ and =*compilation*=:1 ends here

;; [[file:README.org::*Color output in ~eshell~ and =*compilation*=][Color output in ~eshell~ and =*compilation*=:2]]
(add-hook 'eshell-preoutput-filter-functions  #'ansi-color-apply)
;; Color output in ~eshell~ and =*compilation*=:2 ends here

;; [[file:README.org::*Recent files menu][Recent files menu:1]]
(recentf-mode t)

(defun renz/find-recent-file ()
  "Find a file that was recently visted using `completing-read'."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))
;; Recent files menu:1 ends here

;; [[file:README.org::*Fill-column][Fill-column:1]]
(setq-default fill-column 80)
;; Fill-column:1 ends here

;; [[file:README.org::*Scroll bar][Scroll bar:1]]
(scroll-bar-mode -1)
;; Scroll bar:1 ends here

;; [[file:README.org::*Automatically visit symlink sources][Automatically visit symlink sources:1]]
(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)
;; Automatically visit symlink sources:1 ends here

;; [[file:README.org::*Indent with spaces by default][Indent with spaces by default:1]]
(setq-default indent-tabs-mode nil)
;; Indent with spaces by default:1 ends here

;; [[file:README.org::*Enable horizontal scrolling with mouse][Enable horizontal scrolling with mouse:1]]
(setq mouse-wheel-tilt-scroll t)
;; Enable horizontal scrolling with mouse:1 ends here

;; [[file:README.org::*Window management][Window management:1]]
(setq switch-to-buffer-obey-display-actions t)
;; Window management:1 ends here

;; [[file:README.org::*Automatically update buffers when contents change on disk][Automatically update buffers when contents change on disk:1]]
(global-auto-revert-mode)
;; Automatically update buffers when contents change on disk:1 ends here

;; [[file:README.org::*Highlight the line point is on][Highlight the line point is on:1]]
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)
;; Highlight the line point is on:1 ends here

;; [[file:README.org::*Always turn on flymake in prog mode][Always turn on flymake in prog mode:1]]
(add-hook 'prog-mode-hook #'flymake-mode)
;; Always turn on flymake in prog mode:1 ends here

;; [[file:README.org::*Always turn on flymake in prog mode][Always turn on flymake in prog mode:2]]
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; Always turn on flymake in prog mode:2 ends here

;; [[file:README.org::*Automatically create matching parentheses in programming modes][Automatically create matching parentheses in programming modes:1]]
(add-hook 'prog-mode-hook 'electric-pair-local-mode)
(add-hook 'prog-mode-hook 'show-paren-local-mode)
;; Automatically create matching parentheses in programming modes:1 ends here

;; [[file:README.org::*Shorten yes/no prompts to y/n][Shorten yes/no prompts to y/n:1]]
(setq use-short-answers t)
;; Shorten yes/no prompts to y/n:1 ends here

;; [[file:README.org::*Delete whitespace on save][Delete whitespace on save:1]]
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Delete whitespace on save:1 ends here

;; [[file:README.org::*Killing buffers with a running process][Killing buffers with a running process:1]]
(delete 'process-kill-buffer-query-function kill-buffer-query-functions)
;; Killing buffers with a running process:1 ends here

;; [[file:README.org::*Don't wrap lines][Don't wrap lines:1]]
(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))
(add-hook 'shell-mode-hook (lambda () (setq-local truncate-lines nil)))
;; Don't wrap lines:1 ends here

;; [[file:README.org::*Relative line numbers][Relative line numbers:1]]
(defun renz/display-relative-lines ()
  (setq display-line-numbers-width 3)
  (setq display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'renz/display-relative-lines)
(add-hook 'yaml-mode-hook #'renz/display-relative-lines)
(add-hook 'text-mode-hook #'renz/display-relative-lines)
;; Relative line numbers:1 ends here

;; [[file:README.org::*Delete region when we yank on top of it][Delete region when we yank on top of it:1]]
(delete-selection-mode t)
;; Delete region when we yank on top of it:1 ends here

;; [[file:README.org::*Enable mouse in terminal/TTY][Enable mouse in terminal/TTY:1]]
(xterm-mouse-mode 1)
;; Enable mouse in terminal/TTY:1 ends here

;; [[file:README.org::*Compilation][Compilation:1]]
(setq compilation-scroll-output 'first-error)
;; Compilation:1 ends here

;; [[file:README.org::*Tool bar][Tool bar:1]]
(tool-bar-mode -1)
;; Tool bar:1 ends here

;; [[file:README.org::*Tool bar][Tool bar:2]]
(when (renz/windowsp)
  (menu-bar-mode -1))
;; Tool bar:2 ends here

;; [[file:README.org::*Ignore risky .dir-locals.el][Ignore risky .dir-locals.el:1]]
(advice-add 'risky-local-variable-p :override #'ignore)
;; Ignore risky .dir-locals.el:1 ends here

;; [[file:README.org::*=grep= and =find=][=grep= and =find=:1]]
(use-package grep
  :bind ("C-c g" . grep-find)
  :config
  (when (and (executable-find "rg") (renz/windowsp))
    (grep-apply-setting 'grep-find-command
                        '("rg --vimgrep --color never --ignore-case  ." . 42))))
;; =grep= and =find=:1 ends here

;; [[file:README.org::*=grep= and =find=][=grep= and =find=:3]]
(when-let ((on-windows (renz/windowsp))
           (prog-files (getenv "PROGRAMFILES(x86)"))
           (find-prg (expand-file-name "GnuWin32/bin/find.exe" prog-files))
           (find-exists (executable-find find-prg)))
  (setq find-program "C:\\\"Program Files (x86)\"\\GnuWin32\\bin\\find.exe"))
;; =grep= and =find=:3 ends here

;; [[file:README.org::*Confirm when exiting Emacs][Confirm when exiting Emacs:1]]
(setq confirm-kill-emacs 'yes-or-no-p)
;; Confirm when exiting Emacs:1 ends here

;; [[file:README.org::*Smooth scrolling][Smooth scrolling:1]]
(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))
;; Smooth scrolling:1 ends here

;; [[file:README.org::*Spellchecking][Spellchecking:2]]
(cond ((executable-find "aspell")
       (setq ispell-program-name "aspell"
             ispell-really-aspell t))
      ((executable-find "hunspell")
       (setq ispell-program-name "hunspell"
             ispell-really-hunspell t)))
;; Spellchecking:2 ends here

;; [[file:README.org::*Backup and auto-save files][Backup and auto-save files:1]]
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t)
;; Backup and auto-save files:1 ends here

;; [[file:README.org::*Enable ~narrow-to-region~][Enable ~narrow-to-region~:1]]
(put 'narrow-to-region 'disabled nil)
;; Enable ~narrow-to-region~:1 ends here

;; [[file:README.org::*Enable up/downcase-region][Enable up/downcase-region:1]]
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Enable up/downcase-region:1 ends here

;; [[file:README.org::*Mark rings and registers: bigger, faster, stronger][Mark rings and registers: bigger, faster, stronger:1]]
(setq-default mark-ring-max 32)
(setq global-mark-ring-max 32)
;; Mark rings and registers: bigger, faster, stronger:1 ends here

;; [[file:README.org::*Mark rings and registers: bigger, faster, stronger][Mark rings and registers: bigger, faster, stronger:2]]
(set-register ?S '(buffer . "*scratch*"))
(set-register ?I `(file . ,(expand-file-name "README.org" user-emacs-directory)))
(set-register ?B `(file . "~/.bashrc"))
;; Mark rings and registers: bigger, faster, stronger:2 ends here

;; [[file:README.org::*=eldoc=][=eldoc=:1]]
(setq eldoc-echo-area-use-multiline-p nil)
;; =eldoc=:1 ends here

;; [[file:README.org::*~imenu~][~imenu~:1]]
(use-package imenu
  :config
  (setq imenu-auto-rescan t
        org-imenu-depth 3))
;; ~imenu~:1 ends here

;; [[file:README.org::*~dabbrev~][~dabbrev~:1]]
(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
;; ~dabbrev~:1 ends here

;; [[file:README.org::*~dabbrev~][~dabbrev~:2]]
(use-package hippie-exp
  :config
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (delete 'try-expand-line hippie-expand-try-functions-list)
  (delete 'try-complete-lisp-symbol-partially hippie-expand-try-functions-list)
  (delete 'try-complete-lisp-symbol hippie-expand-try-functions-list))
;; ~dabbrev~:2 ends here

;; [[file:README.org::*~dired~][~dired~:1]]
(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-alFh")
  (setq dired-dwim-target t))
;; ~dired~:1 ends here

;; [[file:README.org::*eww - search engine and browser][eww - search engine and browser:1]]
(use-package eww
  :config (setq eww-search-prefix "https://duckduckgo.com/html/?q="))
;; eww - search engine and browser:1 ends here

;; [[file:README.org::*Language Server Protocol (LSP) with ~eglot~][Language Server Protocol (LSP) with ~eglot~:1]]
(use-package eglot
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)
         ("C-c l i" . eglot-inlay-hints-mode)))
;; Language Server Protocol (LSP) with ~eglot~:1 ends here

;; [[file:README.org::*Shell commands][Shell commands:1]]
(defun renz/async-shell-command-filter-hook ()
  "Filter async shell command output via `comint-output-filter'."
  (when (equal (buffer-name (current-buffer)) "*Async Shell Command*")
    ;; When `comint-output-filter' is non-nil, the carriage return characters ^M
    ;; are displayed
    (setq-local comint-inhibit-carriage-motion nil)
    (when-let ((proc (get-buffer-process (current-buffer))))
      ;; Attempting a solution found here:
      ;; https://gnu.emacs.help.narkive.com/2PEYGWfM/m-chars-in-async-command-output
      (set-process-filter proc 'comint-output-filter))))

(add-hook 'shell-mode-hook #'renz/async-shell-command-filter-hook)
;; Shell commands:1 ends here

;; [[file:README.org::*'Uniquify' using slash instead of brackets]['Uniquify' using slash instead of brackets:1]]
(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))
;; 'Uniquify' using slash instead of brackets:1 ends here

;; [[file:README.org::*Save our place in files when we re-visit them][Save our place in files when we re-visit them:1]]
(save-place-mode 1)
;; Save our place in files when we re-visit them:1 ends here

;; [[file:README.org::*Keep existing clipboard text in kill ring][Keep existing clipboard text in kill ring:1]]
(setq save-interprogram-paste-before-kill t)
;; Keep existing clipboard text in kill ring:1 ends here

;; [[file:README.org::*More extensive apropos search][More extensive apropos search:1]]
(setq apropos-do-all t)
;; More extensive apropos search:1 ends here

;; [[file:README.org::*Disable implied frame resizing][Disable implied frame resizing:1]]
(setq frame-inhibit-implied-resize t)
;; Disable implied frame resizing:1 ends here

;; [[file:README.org::*Use one frame for ~ediff~][Use one frame for ~ediff~:1]]
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Use one frame for ~ediff~:1 ends here

;; [[file:README.org::*Prefer newer files on load][Prefer newer files on load:1]]
(setq load-prefer-newer t)
;; Prefer newer files on load:1 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:1]]
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
;; Expanded/better defaults:1 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:2]]
(global-set-key (kbd "C-z") #'zap-up-to-char)
;; Expanded/better defaults:2 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:3]]
(global-set-key [remap list-buffers] 'ibuffer)
;; Expanded/better defaults:3 ends here

;; [[file:README.org::*Expanded/better defaults][Expanded/better defaults:4]]
(use-package flymake
  :bind (:map flymake-mode-map
         ("C-c n" . flymake-goto-next-error)
         ("C-c p" . flymake-goto-prev-error)))
;; Expanded/better defaults:4 ends here

;; [[file:README.org::*=C-c b= build / compile][=C-c b= build / compile:1]]
(global-set-key (kbd "C-c b") #'compile)
(global-set-key (kbd "C-c B") #'recompile)
;; =C-c b= build / compile:1 ends here

;; [[file:README.org::*=C-c c= Insert current dir/file at point][=C-c c= Insert current dir/file at point:1]]
(defun renz/insert-current-dir ()
  "Insert the current `default-directory' at point."
  (interactive)
  (insert default-directory))

(defun renz/insert-current-file ()
  "Insert the current buffer's full file name at point."
  (interactive)
  ;; https://unix.stackexchange.com/a/45381
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(global-set-key (kbd "C-c c d") #'renz/insert-current-dir)
(global-set-key (kbd "C-c c f") #'renz/insert-current-file)
;; =C-c c= Insert current dir/file at point:1 ends here

;; [[file:README.org::*=C-c d= delete pairs of surrounding characters][=C-c d= delete pairs of surrounding characters:1]]
(global-set-key (kbd "C-c d") #'delete-pair)
(setq delete-pair-blink-delay 0.0)
;; =C-c d= delete pairs of surrounding characters:1 ends here

;; [[file:README.org::*=C-c i= browse url of buffer][=C-c i= browse url of buffer:1]]
(global-set-key (kbd "C-c i") #'browse-url-of-buffer)
;; =C-c i= browse url of buffer:1 ends here

;; [[file:README.org::*=C-c j= Toggle window split][=C-c j= Toggle window split:1]]
(defun toggle-window-split ()
  "Switch between horizontal and vertical split window layout."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c j") #'toggle-window-split)
;; =C-c j= Toggle window split:1 ends here

;; [[file:README.org::*=C-c k= kill all but one space][=C-c k= kill all but one space:1]]
(global-set-key (kbd "C-c k") #'bury-buffer)
;; =C-c k= kill all but one space:1 ends here

;; [[file:README.org::*=C-c q= replace regexp][=C-c q= replace regexp:1]]
(global-set-key (kbd "C-c q") #'replace-regexp)
;; =C-c q= replace regexp:1 ends here

;; [[file:README.org::*=C-c r= find recent files][=C-c r= find recent files:1]]
(global-set-key (kbd "C-c r") #'renz/find-recent-file)
;; =C-c r= find recent files:1 ends here

;; [[file:README.org::*=C-c t= visit tags table][=C-c t= visit tags table:1]]
(global-set-key (kbd "C-c t") #'visit-tags-table)
;; =C-c t= visit tags table:1 ends here

;; [[file:README.org::*=C-c s= shell][=C-c s= shell:1]]
(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s e") #'eshell)
;; =C-c s= shell:1 ends here

;; [[file:README.org::*=C-c u= open URL at point in browser][=C-c u= open URL at point in browser:1]]
(global-set-key (kbd "C-c u") #'browse-url-at-point)
;; =C-c u= open URL at point in browser:1 ends here

;; [[file:README.org::*=C-c v= faster git-commit][=C-c v= faster git-commit:1]]
(defun renz/git-commit ()
  (interactive)
  (vc-next-action nil)
  (log-edit-show-diff)
  (other-window 1))

(global-set-key (kbd "C-c v") #'renz/git-commit)
;; =C-c v= faster git-commit:1 ends here

;; [[file:README.org::*=C-c w= whitespace mode][=C-c w= whitespace mode:1]]
(global-set-key (kbd "C-c w") #'whitespace-mode)
;; =C-c w= whitespace mode:1 ends here

;; [[file:README.org::*=C-c x= misc. "execute" commands][=C-c x= misc. "execute" commands:1]]
(global-set-key (kbd "C-c x r") #'restart-emacs)
;; =C-c x= misc. "execute" commands:1 ends here

;; [[file:README.org::*=C-c= Other bindings][=C-c= Other bindings:1]]
(global-set-key (kbd "C-c <DEL>") #'backward-kill-sexp)  ;; TTY-frindly
(global-set-key (kbd "C-c <SPC>") #'mark-sexp)  ;; TTY-friendly
;; =C-c= Other bindings:1 ends here

;; [[file:README.org::*Super bindings][Super bindings:1]]
(global-set-key (kbd "s-p") #'project-switch-project)
;; Super bindings:1 ends here

;; [[file:README.org::*Completion style][Completion style:1]]
(use-package orderless
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; Completion style:1 ends here

;; [[file:README.org::*Nicer Display and Behavior of ~*Completions*~][Nicer Display and Behavior of ~*Completions*~:1]]
(setq completions-format 'one-column)
;; Nicer Display and Behavior of ~*Completions*~:1 ends here

;; [[file:README.org::*Nicer Display and Behavior of ~*Completions*~][Nicer Display and Behavior of ~*Completions*~:2]]
(unless (version< emacs-version "29.0")
  (setq completions-max-height 15))
;; Nicer Display and Behavior of ~*Completions*~:2 ends here

;; [[file:README.org::*Nicer Display and Behavior of ~*Completions*~][Nicer Display and Behavior of ~*Completions*~:3]]
(unless (version< emacs-version "29.0")
  (setq completion-auto-help 'always
        completion-auto-select 'second-tab
        completion-show-help nil
        completions-sort nil
        completions-header-format nil))
;; Nicer Display and Behavior of ~*Completions*~:3 ends here

;; [[file:README.org::*Completion in the minibuffer and at point][Completion in the minibuffer and at point:1]]
(setq tab-always-indent 'complete)
;; Completion in the minibuffer and at point:1 ends here

;; [[file:README.org::*Corfu][Corfu:1]]
(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match 'separator)
  (global-corfu-modes '((not shell-mode) (not eshell-mode) t))
  :config
  (global-corfu-mode))
;; Corfu:1 ends here

;; [[file:README.org::*Vertico][Vertico:1]]
(use-package vertico
  :config
  (vertico-mode))
;; Vertico:1 ends here

;; [[file:README.org::*Marginalia][Marginalia:1]]
(use-package marginalia
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))
;; Marginalia:1 ends here

;; [[file:README.org::*Shell (Bash, sh, ...)][Shell (Bash, sh, ...):1]]
(defun renz/sh-indentation ()
  ;; (setq indent-tabs-mode t)
  (setq tab-width 8))

(add-hook 'sh-mode-hook #'renz/sh-indentation)
(add-hook 'bash-ts-mode-hook #'renz/sh-indentation)
;; Shell (Bash, sh, ...):1 ends here

;; [[file:README.org::*HTML][HTML:1]]
(use-package sgml-mode
  :defer t
  :custom
  (electric-pair-local-mode nil)
  :config
  (let* ((p-tag-old (assoc "p" html-tag-alist))
         ;; Close the <p> tag and open on a new line.
         (p-tag-new `("p" \n ,(cdr (cdr p-tag-old)))))
    (add-to-list 'html-tag-alist p-tag-new)
    ;; Close the <code> tag and stay inline.
    (add-to-list 'html-tag-alist '("code"))))
;; HTML:1 ends here

;; [[file:README.org::*CSS][CSS:1]]
(setq css-indent-offset 2)
;; CSS:1 ends here

;; [[file:README.org::*Org-mode][Org-mode:1]]
(setq renz/org-home "~/.emacs.d/org/")
(put 'org-publish-project-alist 'safe-local-variable #'listp)
(put 'org-html-validation-link 'safe-local-variable #'symbolp)
(put 'org-html-head-include-scripts 'safe-local-variable #'symbolp)
(put 'org-html-head-include-default-style 'safe-local-variable #'symbolp)
(put 'org-html-head 'safe-local-variable #'stringp)
;; Org-mode:1 ends here

;; [[file:README.org::*Org-mode][Org-mode:2]]
(defun renz/org-babel-tangle-jump-to-src ()
  "The opposite of `org-babel-tangle-jump-to-org'.
Jumps to an Org src block from tangled code."
  (interactive)
  (if (org-in-block-p)
      (let* ((header (car (org-babel-tangle-single-block 1 'only-this-block)))
             (tangle (car header))
             (lang (caadr header))
             (buffer (nth 2 (cadr header)))
             (org-id (nth 3 (cadr header)))
             (source-name (nth 4 (cadr header)))
             (search-comment (org-fill-template
                              org-babel-tangle-comment-format-beg
                              `(("link" . ,org-id) ("source-name" . ,source-name))))
             (file (expand-file-name
                    (org-babel-effective-tangled-filename buffer lang tangle))))
        (if (not (file-exists-p file))
            (message "File does not exist. 'org-babel-tangle' first to create file.")
          (find-file file)
          (beginning-of-buffer)
          (search-forward search-comment)))
    (message "Cannot jump to tangled file because point is not at org src block.")))
;; Org-mode:2 ends here

;; [[file:README.org::*Org-mode][Org-mode:3]]
(defun renz/list-files-with-absolute-path (directory)
  "Return a list of files in DIRECTORY with their absolute paths."
  (cl-remove-if-not #'file-regular-p (directory-files directory t ".*\.org$")))

(use-package org
  :hook
  ((org-mode . (lambda () (progn
                            (add-hook 'after-save-hook #'org-babel-tangle :append :local)
                            (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors)
                            (setq indent-tabs-mode nil)))))

  :init
  (defun renz/jump-org ()
    "Prompt for an org file in my emacs directory, then go there."
    (interactive)
    (renz/--jump-section renz/org-home "Org files: " ".*\.org$"))

  :bind
  (("C-c o a" . org-agenda)
   ("C-c o b d" . org-babel-detangle)
   ("C-c o b o" . org-babel-tangle-jump-to-org)
   ("C-c o b s" . renz/org-babel-tangle-jump-to-src)
   ("C-c o k" . org-babel-remove-result)
   ("C-c o o" . renz/jump-org)
   ("C-c o y" . ox-clip-image-to-clipboard))

  :custom
  (org-image-actual-width nil "Enable resizing of images")
  (org-agenda-files (renz/list-files-with-absolute-path renz/org-home) "Sources for Org agenda view")
  (org-html-htmlize-output-type nil "See C-h f org-html-htmlize-output-type")
  (org-confirm-babel-evaluate nil "Don't ask for confirmation when executing src blocks")
  (org-goto-interface 'outline-path-completion "Use completing-read for org-goto (C-c C-j, nicer than imenu)")
  (org-outline-path-complete-in-steps nil "Flatten the outline path, instead of completing hierarchically")

  :config
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (shell . t)
     (R . t)
     ;; (fortran . t)
     ;; (julia . t)
     ;; (jupyter . t)
     ;; (scheme . t)
     ;; (haskell . t)
     (lisp . t)
     ;; (clojure . t)
     ;; (C . t)
     ;; (org . t)
     ;; (gnuplot . t)
     ;; (awk . t)
     ;; (latex . t)
     )))
;; Org-mode:3 ends here

;; [[file:README.org::*DDL is SQL][DDL is SQL:1]]
(add-to-list 'auto-mode-alist '("\\.ddl\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.bql\\'" . sql-mode))
;; DDL is SQL:1 ends here

;; [[file:README.org::*Indentation][Indentation:2]]
(defun renz/sql-mode-hook ()
  (setq tab-width 4))

(defvar renz/sql-indentation-offsets-alist
  '((syntax-error sqlind-report-sytax-error)
    (in-string sqlind-report-runaway-string)
    (comment-continuation sqlind-indent-comment-continuation)
    (comment-start sqlind-indent-comment-start)
    (toplevel 0)
    (in-block +)
    (in-begin-block +)
    (block-start 0)
    (block-end 0)
    (declare-statement +)
    (package ++)
    (package-body 0)
    (create-statement +)
    (defun-start +)
    (labeled-statement-start 0)
    (statement-continuation +)
    (nested-statement-open sqlind-use-anchor-indentation +)
    (nested-statement-continuation sqlind-use-previous-line-indentation)
    (nested-statement-close sqlind-use-anchor-indentation)
    (with-clause sqlind-use-anchor-indentation)
    (with-clause-cte +)
    (with-clause-cte-cont ++)
    (case-clause 0)
    (case-clause-item sqlind-use-anchor-indentation +)
    (case-clause-item-cont sqlind-right-justify-clause)
    (select-clause 0)
    (select-column sqlind-indent-select-column)
    (select-column-continuation sqlind-indent-select-column +)
    (select-join-condition ++)
    (select-table sqlind-indent-select-table)
    (select-table-continuation sqlind-indent-select-table +)
    (in-select-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (insert-clause 0)
    (in-insert-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (delete-clause 0)
    (in-delete-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (update-clause 0)
    (in-update-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)))

(defun renz/sql-indentation-offsets ()
  (setq sqlind-indentation-offsets-alist
        renz/sql-indentation-offsets-alist)
  (setq sqlind-basic-offset 4))

(use-package sql-indent
  :hook (sqlind-minor-mode . renz/sql-indentation-offsets))

(use-package sql-mode
  :hook ((sql-mode . renz/sql-mode-hook)
         (sql-mode . sqlup-mode)
         (sql-mode . sqlind-minor-mode)))
;; Indentation:2 ends here

;; [[file:README.org::*Interactive ~hive2~ mode][Interactive ~hive2~ mode:1]]
(use-package hive2
  :load-path "site-lisp/"
  :demand t
  :mode ("\\.hql" . sql-mode))
;; Interactive ~hive2~ mode:1 ends here

;; [[file:README.org::*Interactive =bq shell=][Interactive =bq shell=:3]]
(use-package bq
  :load-path "site-lisp"
  :demand t)
;; Interactive =bq shell=:3 ends here

;; [[file:README.org::*BigQuery ~sql~ Blocks in Org-Babel][BigQuery ~sql~ Blocks in Org-Babel:1]]
(defun org-babel-execute:bq (orig-fun body params)
  (if (string-equal-ignore-case (cdr (assq :engine params)) "bq")
      (json-to-org-table-parse-json-string
       (org-babel-execute:shell (concat "bq query --format=json --nouse_legacy_sql '" body "'")
                                params))
    (org-babel-execute:sql body params)))

(advice-add 'org-babel-execute:sql :around #'org-babel-execute:bq)
;; BigQuery ~sql~ Blocks in Org-Babel:1 ends here

;; [[file:README.org::*Pipfiles are TOML][Pipfiles are TOML:1]]
(add-to-list 'auto-mode-alist '("Pipfile" . toml-ts-mode))
;; Pipfiles are TOML:1 ends here

;; [[file:README.org::*Ignore =.venv= in VC operations][Ignore =.venv= in VC operations:1]]
(add-to-list 'vc-directory-exclusion-list ".venv")
;; Ignore =.venv= in VC operations:1 ends here

;; [[file:README.org::*Flatten items in =imenu=][Flatten items in =imenu=:1]]
(add-hook 'python-mode-hook
          (lambda () (setq-local imenu-create-index-function
                                 'python-imenu-create-flat-index)))

(add-hook 'python-ts-mode-hook
          (lambda () (setq-local imenu-create-index-function
                                 'python-imenu-treesit-create-flat-index)))
;; Flatten items in =imenu=:1 ends here

;; [[file:README.org::*Interactively setting the virtual environment for =pyrightconfig.json=][Interactively setting the virtual environment for =pyrightconfig.json=:1]]
(defun pyrightconfig-write (virtualenv)
  "Write a `pyrightconfig.json' file at the Git root of a project
with `venvPath' and `venv' set to the absolute path of
`virtualenv'.  When run interactively, prompts for a directory to
select."
  (interactive "DEnv: ")
  ;; Naming convention for venvPath matches the field for pyrightconfig.json
  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))
    (with-temp-file out-file (insert out-contents))
    (message (concat "Configured `" out-file "` to use environment `" venv-dir))))
;; Interactively setting the virtual environment for =pyrightconfig.json=:1 ends here

;; [[file:README.org::*Pyright error links in =*compilation*=][Pyright error links in =*compilation*=:1]]
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))
;; Pyright error links in =*compilation*=:1 ends here

;; [[file:README.org::*Python check with "ruff"][Python check with "ruff":1]]
(use-package python
  :config
  (require 'eglot)
  (setq python-check-command "ruff check")
  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-ts-mode-hook #'flymake-mode))
;; Python check with "ruff":1 ends here

;; [[file:README.org::*Make check command and virtualenv root safe for .dir-locals.el][Make check command and virtualenv root safe for .dir-locals.el:2]]
(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
(put 'python-interpreter 'safe-local-variable #'stringp)
;; Make check command and virtualenv root safe for .dir-locals.el:2 ends here

;; [[file:README.org::*Markdown][Markdown:1]]
(when (and (not (executable-find "markdown")) (executable-find "markdown_py"))
  (setq markdown-command "markdown_py"))
;; Markdown:1 ends here

;; [[file:README.org::*Markdown][Markdown:2]]
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
;; Markdown:2 ends here

;; [[file:README.org::*Markdown][Markdown:3]]
(setq markdown-fontify-code-blocks-natively t)
;; Markdown:3 ends here

;; [[file:README.org::*csv-mode][csv-mode:1]]
(use-package csv-mode
  :mode "\\.csv\\'")
;; csv-mode:1 ends here

;; [[file:README.org::*JavaScript][JavaScript:1]]
(use-package js
  :config
  (setq js-mode-map (define-keymap "M-." #'xref-find-definitions))
  (setq js-ts-mode-map (copy-keymap js-mode-map)))
;; JavaScript:1 ends here

;; [[file:README.org::*Eshell][Eshell:1]]
(use-package eshell
  :custom
  (eshell-visual-commands '("make" "vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm"))
  (eshell-visual-subcommands '(("git" "log" "diff" "show")
                               ("micromamba" "install" "update" "upgrade" "create" "run" "self-update")
                               ("mamba" "install" "update" "upgrade")
                               ("poetry" "install" "update" "upgrade")
                               ("docker" "build")
                               ("uv" "pip"))))
;; Eshell:1 ends here

;; [[file:README.org::*=treesit-auto=: Automatically Using TreeSitter Modes][=treesit-auto=: Automatically Using TreeSitter Modes:1]]
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(awk bash c css go html make markdown r ruby rust toml yaml))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;; =treesit-auto=: Automatically Using TreeSitter Modes:1 ends here

;; [[file:README.org::*=pyvenv=][=pyvenv=:1]]
(use-package pyvenv
  ;; Overrides `mark-page'
  :bind (("C-x p a" . pyvenv-activate)
         ("C-x p u" . pyvenv-deactivate))
  :config
  (put 'pyvenv-mode 'safe-local-variable #'stringp)
  (pyvenv-tracking-mode 1)
  (pyvenv-mode 1))
;; =pyvenv=:1 ends here

;; [[file:README.org::*=direnv= Managing project environment variables][=direnv= Managing project environment variables:1]]
(use-package direnv
  :config (direnv-mode))
;; =direnv= Managing project environment variables:1 ends here

;; [[file:README.org::*Cloud stuff][Cloud stuff:1]]
(defun renz/glogin ()
  "Log in to GCP"
  (interactive)
  (shell-command "gcloud auth login --update-adc"))
;; Cloud stuff:1 ends here

(provide 'init.el)
;;; init.el ends here
