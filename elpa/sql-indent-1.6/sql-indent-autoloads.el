;;; sql-indent-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sql-indent" "sql-indent.el" (0 0 0 0))
;;; Generated autoloads from sql-indent.el

(autoload 'sqlind-minor-mode "sql-indent" "\
Toggle SQL syntactic indentation on or off.
With syntactic indentation, hitting TAB on a line in a SQL buffer
will indent the line according to the syntactic context of the
SQL statement being edited.

This is a minor mode.  If called interactively, toggle the
`sqlind minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sqlind-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

A set of alignment rules are also enabled with this minor mode.
Selecting a region of text and typing `M-x align RET` will align
the statements.  This can be used, for example, to align the 'as'
column aliases in select statements.

\(fn &optional ARG)" t nil)

(autoload 'sqlind-setup "sql-indent" "\
Enable SQL syntactic indentation unconditionally.
This function is deprecated, consider using the function
`sqlind-minor-mode' instead." nil nil)

(register-definition-prefixes "sql-indent" '("sqlind-"))

;;;***

;;;### (autoloads nil "sql-indent-left" "sql-indent-left.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from sql-indent-left.el

(autoload 'sqlind-setup-style-left "sql-indent-left" "\
Define an sql-indentation style where keywords are left aligned." t nil)

(autoload 'sqlind-setup-style-right "sql-indent-left" "\
Define an sql-indentation style where keywords are right aligned." t nil)

(autoload 'sqlind-setup-style-default "sql-indent-left" "\
Define an sql-indentation style where keywords are right aligned." t nil)

(register-definition-prefixes "sql-indent-left" '("indent-case-statement-items" "sqlind-indent"))

;;;***

;;;### (autoloads nil nil ("sql-indent-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sql-indent-autoloads.el ends here
