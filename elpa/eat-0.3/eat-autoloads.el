;;; eat-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eat" "eat.el" (0 0 0 0))
;;; Generated autoloads from eat.el

(autoload 'eat-term-make "eat" "\
Make a Eat terminal at POSITION in BUFFER.

\(fn BUFFER POSITION)" nil nil)

(autoload 'eat "eat" "\
Start a new Eat terminal emulator in a buffer.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like \\[universal-argument] 42 \\[eshell]),
switch to the session with that number, or create it if it doesn't
already exist.

PROGRAM can be a shell command.

\(fn &optional PROGRAM ARG)" t nil)

(defvar eat-eshell-mode nil "\
Non-nil if Eat-Eshell mode is enabled.
See the `eat-eshell-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eat-eshell-mode'.")

(custom-autoload 'eat-eshell-mode "eat" nil)

(autoload 'eat-eshell-mode "eat" "\
Toggle Eat terminal emulation is Eshell.

This is a minor mode.  If called interactively, toggle the `Eat-Eshell mode'
mode.  If the prefix argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the mode if ARG
is nil, omitted, or is a positive number.  Disable the mode if ARG is a negative
number.

To check whether the minor mode is enabled in the current buffer, evaluate
`(default-value \\='eat-eshell-mode)'.

The mode's hook is called both when the mode is enabled and when it is disabled.

\(fn &optional ARG)" t nil)

(defvar eat-eshell-visual-command-mode nil "\
Non-nil if Eat-Eshell-Visual-Command mode is enabled.
See the `eat-eshell-visual-command-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eat-eshell-visual-command-mode'.")

(custom-autoload 'eat-eshell-visual-command-mode "eat" nil)

(autoload 'eat-eshell-visual-command-mode "eat" "\
Toggle running Eshell visual commands with Eat.

This is a minor mode.  If called interactively, toggle the
`Eat-Eshell-Visual-Command mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the mode if ARG
is nil, omitted, or is a positive number.  Disable the mode if ARG is a negative
number.

To check whether the minor mode is enabled in the current buffer, evaluate
`(default-value \\='eat-eshell-visual-command-mode)'.

The mode's hook is called both when the mode is enabled and when it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'eat-project "eat" "\
Start Eat in the current project's root directory.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like \\[universal-argument] 42 \\[eshell]),
switch to the session with that number, or create it if it doesn't
already exist.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "eat" '("eat-"))

;;;***

;;;### (autoloads nil nil ("eat-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eat-autoloads.el ends here
