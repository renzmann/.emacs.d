;;; ahk-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ahk-mode" "ahk-mode.el" (0 0 0 0))
;;; Generated autoloads from ahk-mode.el

(add-to-list 'auto-mode-alist '("\\.ahk\\'" . ahk-mode))

(autoload 'ahk-mode "ahk-mode" "\
Major mode for editing AutoHotkey script (AHK).

The hook functions in `ahk-mode-hook' are run after mode initialization.

Key Bindings
\\{ahk-mode-map}

\(fn)" t nil)

(register-definition-prefixes "ahk-mode" '("ac-source-" "ahk-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ahk-mode-autoloads.el ends here
