;;; treesit-auto-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from treesit-auto.el

(autoload 'treesit-auto-apply-remap "treesit-auto" "\
Adjust `major-mode-remap-alist' using installed tree-sitter grammars.")
(defvar global-treesit-auto-mode nil "\
Non-nil if Global Treesit-Auto mode is enabled.
See the `global-treesit-auto-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-treesit-auto-mode'.")
(custom-autoload 'global-treesit-auto-mode "treesit-auto" nil)
(autoload 'global-treesit-auto-mode "treesit-auto" "\
Toggle `global-treesit-auto-mode'.

This is a global minor mode.  If called interactively, toggle the
`Global Treesit-Auto mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='global-treesit-auto-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "treesit-auto" '("treesit-auto-"))

;;; End of scraped data

(provide 'treesit-auto-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; treesit-auto-autoloads.el ends here
