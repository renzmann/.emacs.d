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
(defun renzmann-manage-packages ()
  "Interactively manage the package-selected-packages variable."
  (interactive)
  (customize-variable 'package-selected-packages))
(global-set-key (kbd "C-c p") 'renzmann-manage-packages)

(defun renzmann-open-init ()
  "Go to my init.el file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c i") 'renzmann-open-init)

(defun renzmann-open-keymaps ()
  "Go to my keymaps.el file"
  (interactive)
  (find-file "~/.emacs.d/keymaps.el"))
(global-set-key (kbd "C-c k") 'renzmann-open-keymaps)

;; Reserved for users: f5 - f8
(global-set-key (kbd "S-<f5>") 'find-file-at-point)
(global-set-key (kbd "<f6>") 'find-function-at-point)
(global-set-key (kbd "S-<f6>") 'find-symbol-at-point)
(global-set-key (kbd "<f8>") 'writeroom-mode)
(global-set-key [remap list-buffers] 'ibuffer)
;; (global-set-key [remap minibuffer-complete] 'icomplete-force-complete)
