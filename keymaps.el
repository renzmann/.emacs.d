;; Keybindings / keymaps
;; ============================================================================
;; Better defaults
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c d") 'flymake-show-buffer-diagnostics)

;; Reserved for users: C-c <letter>
;; (global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-c p") (lambda () (interactive) (customize-variable 'package-selected-packages)))

;; Reserved for users: f5 - f8
(global-set-key (kbd "<C-c i>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "S-<f5>") 'find-file-at-point)
(global-set-key (kbd "<f6>") 'find-function-at-point)
(global-set-key (kbd "S-<f6>") 'find-symbol-at-point)
(global-set-key (kbd "<f8>") 'writeroom-mode)
(global-set-key [remap list-buffers] 'ibuffer)
