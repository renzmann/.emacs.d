;;; ef-themes-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ef-themes" "ef-themes.el" (0 0 0 0))
;;; Generated autoloads from ef-themes.el

(autoload 'ef-themes-select "ef-themes" "\
Load an Ef THEME using minibuffer completion.
When called from Lisp, THEME is a symbol.

\(fn THEME)" t nil)

(autoload 'ef-themes-toggle "ef-themes" "\
Toggle between the two `ef-themes-to-toggle'." t nil)

(autoload 'ef-themes-load-random "ef-themes" "\
Load an Ef theme at random, excluding the current one.
With optional VARIANT as either `light' or `dark', limit the set
to the relevant themes.

When called interactively, VARIANT is the prefix argument which
prompts with completion for either `light' or `dark'.

\(fn &optional VARIANT)" t nil)

(autoload 'ef-themes-preview-colors "ef-themes" "\
Preview palette of the Ef THEME of choice.

\(fn THEME)" t nil)

(autoload 'ef-themes-preview-colors-current "ef-themes" "\
Call `ef-themes-preview-colors' for the current Ef theme." t nil)

(autoload 'ef-themes-theme "ef-themes" "\
Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `ef-themes-faces' and
`ef-themes-custom-variables' respectively.

\(fn NAME PALETTE)" nil t)

(function-put 'ef-themes-theme 'lisp-indent-function '0)

(when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (file-equal-p dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir))))

(register-definition-prefixes "ef-themes" '("ef-themes-"))

;;;***

;;;### (autoloads nil nil ("ef-autumn-theme.el" "ef-dark-theme.el"
;;;;;;  "ef-day-theme.el" "ef-deuteranopia-dark-theme.el" "ef-deuteranopia-light-theme.el"
;;;;;;  "ef-light-theme.el" "ef-night-theme.el" "ef-spring-theme.el"
;;;;;;  "ef-summer-theme.el" "ef-themes-pkg.el" "ef-winter-theme.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ef-themes-autoloads.el ends here
