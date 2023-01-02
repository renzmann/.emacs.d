;;; scala-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "scala-compile" "scala-compile.el" (0 0 0 0))
;;; Generated autoloads from scala-compile.el

(autoload 'scala-compile "scala-compile" "\
`compile' specialised to Scala.

First use in a buffer or calling with a prefix will prompt for a
command, otherwise the last command is used.

The command history is global.

A universal argument will invoke `scala-compile-alt', which
will cause the subsequent call to prompt.

A prefix argument will ensure that the user is prompted to
confirm the selection.

A string argument will run the command (for scripting).

\(fn &optional EDIT-COMMAND)" t nil)

(register-definition-prefixes "scala-compile" '("scala-"))

;;;***

;;;### (autoloads nil "scala-mode" "scala-mode.el" (0 0 0 0))
;;; Generated autoloads from scala-mode.el

(autoload 'scala-mode:set-scala-syntax-mode "scala-mode" "\
Sets the syntax-table and other related variables for the current buffer to those of scala-mode. Can be used to make some other major mode (such as sbt-mode) use scala syntax-table." nil nil)

(autoload 'scala-mode:goto-start-of-code "scala-mode" "\
Go to the start of the real code in the file: object, class or trait." t nil)

(autoload 'scala-mode "scala-mode" "\
Major mode for editing scala code.

When started, runs `scala-mode-hook'.

\\{scala-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" . scala-mode))

(modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" 'utf-8)

(register-definition-prefixes "scala-mode" '("scala-mode:"))

;;;***

;;;### (autoloads nil "scala-mode-fontlock" "scala-mode-fontlock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from scala-mode-fontlock.el

(register-definition-prefixes "scala-mode-fontlock" '("scala-font-lock:"))

;;;***

;;;### (autoloads nil "scala-mode-imenu" "scala-mode-imenu.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from scala-mode-imenu.el

(register-definition-prefixes "scala-mode-imenu" '("scala-imenu:"))

;;;***

;;;### (autoloads nil "scala-mode-indent" "scala-mode-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from scala-mode-indent.el

(register-definition-prefixes "scala-mode-indent" '("scala-"))

;;;***

;;;### (autoloads nil "scala-mode-lib" "scala-mode-lib.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from scala-mode-lib.el

(register-definition-prefixes "scala-mode-lib" '("scala-"))

;;;***

;;;### (autoloads nil "scala-mode-map" "scala-mode-map.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from scala-mode-map.el

(register-definition-prefixes "scala-mode-map" '("scala-mode-map"))

;;;***

;;;### (autoloads nil "scala-mode-paragraph" "scala-mode-paragraph.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from scala-mode-paragraph.el

(register-definition-prefixes "scala-mode-paragraph" '("scala-paragraph:"))

;;;***

;;;### (autoloads nil "scala-mode-syntax" "scala-mode-syntax.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from scala-mode-syntax.el

(register-definition-prefixes "scala-mode-syntax" '("scala-syntax:"))

;;;***

;;;### (autoloads nil "scala-organise" "scala-organise.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from scala-organise.el

(register-definition-prefixes "scala-organise" '("scala-organise"))

;;;***

;;;### (autoloads nil nil ("scala-mode-pkg.el" "scala-mode-prettify-symbols.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; scala-mode-autoloads.el ends here
