;;; coterm-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "coterm" "coterm.el" (0 0 0 0))
;;; Generated autoloads from coterm.el

(defvar coterm-mode nil "\
Non-nil if Coterm mode is enabled.
See the `coterm-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `coterm-mode'.")

(custom-autoload 'coterm-mode "coterm" nil)

(autoload 'coterm-mode "coterm" "\
Improved terminal emulation in comint processes.
When this mode is enabled, terminal emulation is enabled for all
newly spawned comint processes, allowing you to use more complex
console programs such as \"less\" and \"mpv\" and full-screen
programs such as \"vi\", \"top\", \"htop\" or even \"emacs -nw\".

This is a minor mode.  If called interactively, toggle the
`Coterm mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='coterm-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Environment variables for comint processes are set according to
variables `coterm-term-name' and `coterm-termcap-format'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "coterm" '("comint-exec-1" "coterm-"))

;;;***

;;;### (autoloads nil nil ("coterm-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; coterm-autoloads.el ends here
