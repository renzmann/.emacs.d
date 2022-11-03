;;; org-modern-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-modern" "org-modern.el" (0 0 0 0))
;;; Generated autoloads from org-modern.el

(autoload 'org-modern-mode "org-modern" "\
Modern looks for Org.

This is a minor mode.  If called interactively, toggle the
`Org-Modern mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-modern-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'org-modern-agenda "org-modern" "\
Finalize Org agenda highlighting." nil nil)

(put 'global-org-modern-mode 'globalized-minor-mode t)

(defvar global-org-modern-mode nil "\
Non-nil if Global Org-Modern mode is enabled.
See the `global-org-modern-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-org-modern-mode'.")

(custom-autoload 'global-org-modern-mode "org-modern" nil)

(autoload 'global-org-modern-mode "org-modern" "\
Toggle Org-Modern mode in all buffers.
With prefix ARG, enable Global Org-Modern mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Org-Modern mode is enabled in all buffers where `org-modern--on' would
do it.

See `org-modern-mode' for more information on Org-Modern mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-modern" '("org-modern-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-modern-autoloads.el ends here
