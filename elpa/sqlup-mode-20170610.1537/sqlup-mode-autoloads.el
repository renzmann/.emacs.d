;;; sqlup-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sqlup-mode" "sqlup-mode.el" (0 0 0 0))
;;; Generated autoloads from sqlup-mode.el

(autoload 'sqlup-mode "sqlup-mode" "\
Capitalizes SQL keywords for you.

This is a minor mode.  If called interactively, toggle the `Sqlup
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sqlup-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'sqlup-capitalize-keywords-in-region "sqlup-mode" "\
Call this function on a region to capitalize the SQL keywords therein.

\(fn START-POS END-POS)" t nil)

(register-definition-prefixes "sqlup-mode" '("sqlup-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sqlup-mode-autoloads.el ends here
