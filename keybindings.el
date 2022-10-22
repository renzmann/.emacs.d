;; ============================================================================
;;                           Keybindings
;; ============================================================================
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html

;; ----------------------------------------
;; Keybound functions
;; ----------------------------------------
(defun renz/jump-configuration ()
  (interactive)
  (find-file
   (concat "~/.emacs.d/"
           (completing-read "Elisp config files: "
                            (directory-files "~/.emacs.d/" nil ".*\.el$")))))

(defun renz/jump-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun renz/jump-keybindings ()
  (interactive)
  (find-file "~/.emacs.d/keybindings.el"))

;; ----------------------------------------
;; Expanded defaults
;; ----------------------------------------
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; UNBINDS the suspend-frame keybinding, which is annoying on GUI.
;; Use C-x C-z if you really want to suspend the frame.
(global-set-key (kbd "C-z") #'zap-up-to-char)

;; A better version of `dabbrev'
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
;; I tend to prefer this on its own key, since I like a lot of the
;; default dabbrev behavior.
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Better buffer list for C-x C-b
(global-set-key [remap list-buffers] 'ibuffer)

;; When flycheck is running (usually from a language server), bind next/previous
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; When using isearch to jump to things, use C-RET to put point on the
;; opposite side of where it would normally end up
(define-key isearch-mode-map (kbd "<C-return>")
  (defun isearch-done-opposite (&optional nopush edit)
    "End current search in the opposite side of the match."
    (interactive)
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end (goto-char isearch-other-end))))

;; ----------------------------------------
;; C-c <letter> bindings
;; ----------------------------------------
(global-set-key (kbd "C-c a") #'org-agenda)
;; (global-set-key (kbd "C-c b") #')
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c d") #'renz/find-tag)
;; (global-set-key (kbd "C-c e") #')
(global-set-key (kbd "C-c f") #'hippie-expand)
;; (global-set-key (kbd "C-c g") #')
;; (global-set-key (kbd "C-c h") #')
(require 'change-inner)
(global-set-key (kbd "C-c i") #'change-inner)
(global-set-key (kbd "C-c j") #'imenu)  ; matches major modes that use C-c C-j
;; (global-set-key (kbd "C-c k") #')
(global-set-key (kbd "C-c l") #'eglot)
(global-set-key (kbd "C-c m") #'ef-themes-toggle)
(global-set-key (kbd "C-c n") #'minimap-mode)
(global-set-key (kbd "C-c o") #'change-outer)
(global-set-key (kbd "C-c p") #'projectile-command-map)
;; (global-set-key (kbd "C-c q") #')
(global-set-key (kbd "C-c r") #'renz/recentf-find-file)
(global-set-key (kbd "C-c s s") #'renz/jump-configuration)
(global-set-key (kbd "C-c s i") #'renz/jump-init)
(global-set-key (kbd "C-c s k") #'renz/jump-keybindings)
(global-set-key (kbd "C-c t d") #'org-babel-detangle)
(global-set-key (kbd "C-c t o") #'org-babel-tangle-jump-to-org)
(global-set-key (kbd "C-c t s") #'renz/org-babel-tangle-jump-to-src)
;; (global-set-key (kbd "C-c u") #')
;; (global-set-key (kbd "C-c v") #')
(global-set-key (kbd "C-c w") #'renz/org-kill-src-block)
;; (global-set-key (kbd "C-c x") #')
;; (global-set-key (kbd "C-c y") #')
;; (global-set-key (kbd "C-c z") #')

;; ----------------------------------------
;; F5 - F9
;; ----------------------------------------
(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "M-<f5>") #'recompile)
;; (global-set-key (kbd "<f6>") #')
;; (global-set-key (kbd "M-<f6>") #')
;; (global-set-key (kbd "<f7>") #')
;; (global-set-key (kbd "M-<f7>") #')
;; (global-set-key (kbd "<f8>") #')
;; (global-set-key (kbd "M-<f8>") #')
(global-set-key (kbd "<f9>") #'vterm)
(global-set-key (kbd "M-<f9>") #'eshell)
(global-set-key (kbd "S-<f9>") #'ansi-term)
(global-set-key (kbd "s-<f9>") #'shell)

;; ----------------------------------------
;; Nonstandard bindings
;; ----------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-=") #'er/expand-region)

(when (eq system-type 'darwin)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
