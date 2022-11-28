;;; kind-icon-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kind-icon" "kind-icon.el" (0 0 0 0))
;;; Generated autoloads from kind-icon.el

(autoload 'kind-icon-margin-formatter "kind-icon" "\
Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list.

\(fn METADATA)" nil nil)

(autoload 'kind-icon-enhance-completion "kind-icon" "\
A wrapper for completion-in-region-functions.
This wrapper sets a custom affixation-function which places an
icon in the prefix slot. Use it like:

  (setq completion-in-region-function 
     (kind-icon-enhance-completion 
       completion-in-region-function))

\(fn COMPLETION-FUNCTION)" nil nil)

(register-definition-prefixes "kind-icon" '("kind-icon-"))

;;;***

;;;### (autoloads nil nil ("kind-icon-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kind-icon-autoloads.el ends here
