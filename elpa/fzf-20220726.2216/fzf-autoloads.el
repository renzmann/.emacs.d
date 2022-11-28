;;; fzf-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fzf" "fzf.el" (0 0 0 0))
;;; Generated autoloads from fzf.el

(autoload 'fzf "fzf" "\
Starts a fzf session." t nil)

(autoload 'fzf-with-entries "fzf" "\
Run `fzf` with the list ENTRIES as input.

ACTION is a function that takes a single argument, which is the
selected result from `fzf`. DIRECTORY is the directory to start in

\(fn ENTRIES ACTION &optional DIRECTORY)" t nil)

(autoload 'fzf-directory "fzf" "\
Starts a fzf session at the specified directory." t nil)

(autoload 'fzf-switch-buffer "fzf" nil t nil)

(autoload 'fzf-find-file "fzf" "\


\(fn &optional DIRECTORY)" t nil)

(autoload 'fzf-find-file-in-dir "fzf" "\


\(fn DIRECTORY)" t nil)

(autoload 'fzf-git-grep "fzf" "\
Starts a fzf session based on git grep result. The input comes
   from the prompt or the selected region." t nil)

(autoload 'fzf-recentf "fzf" nil t nil)

(autoload 'fzf-grep "fzf" "\
Call `fzf/grep-command` on SEARCH.

If SEARCH is nil, read input interactively.
Grep in `fzf/resolve-directory` using DIRECTORY if provided.
If AS-FILTER is non-nil, use grep as the narrowing filter instead of fzf.

\(fn &optional SEARCH DIRECTORY AS-FILTER)" t nil)

(autoload 'fzf-grep-in-dir "fzf" "\
Call `fzf-grep` in DIRECTORY.

If DIRECTORY is nil, read input interactively.
If AS-FILTER is non-nil, use grep as the narrowing filter instead of fzf.

\(fn &optional DIRECTORY AS-FILTER)" t nil)

(autoload 'fzf-grep-with-narrowing "fzf" "\
Call `fzf-grep` with grep as the narrowing filter." t nil)

(autoload 'fzf-grep-in-dir-with-narrowing "fzf" "\
Call `fzf-grep-in-dir` with grep as the narrowing filter." t nil)

(autoload 'fzf-grep-dwim "fzf" "\
Call `fzf-grep` on `symbol-at-point`.

If `thing-at-point` is not a symbol, read input interactively." t nil)

(autoload 'fzf-grep-dwim-with-narrowing "fzf" "\
Call `fzf-grep` on `symbol-at-point`, with grep as the narrowing filter.

If `thing-at-point` is not a symbol, read input interactively." t nil)

(autoload 'fzf-git "fzf" "\
Starts an fzf session at the root of the current git project." t nil)

(autoload 'fzf-hg "fzf" "\
Starts an fzf session at the root of the current hg project." t nil)

(autoload 'fzf-git-files "fzf" "\
Starts an fzf session for tracked files in the current git project." t nil)

(autoload 'fzf-projectile "fzf" "\
Starts an fzf session at the root of the current projectile project." t nil)

(register-definition-prefixes "fzf" '("fzf"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fzf-autoloads.el ends here
