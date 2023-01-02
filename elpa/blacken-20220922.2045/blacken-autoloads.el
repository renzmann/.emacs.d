;;; blacken-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "blacken" "blacken.el" (0 0 0 0))
;;; Generated autoloads from blacken.el

(autoload 'blacken-buffer "blacken" "\
Try to blacken the current buffer.

Show black output, if black exit abnormally and DISPLAY is t.

\(fn &optional DISPLAY)" t nil)

(autoload 'blacken-mode "blacken" "\
Automatically run black before saving.

This is a minor mode.  If called interactively, toggle the
`Blacken mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `blacken-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "blacken" '("blacken-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; blacken-autoloads.el ends here
