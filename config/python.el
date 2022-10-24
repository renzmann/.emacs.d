
;; ============================================================================
;;                              Python
;; ============================================================================
;; Example error from pyright
;; --------------------------
;; /home/robb/tmp/errors.py/
;;   /home/robb/tmp/errors.py:1:1 - error: "foo" is not defined (reportUndefinedVariable)
;;   /home/robb/tmp/errors.py:1:1 - warning: Expression value is unused (reportUnusedExpression)
;;   /home/robb/tmp/errors.py:4:12 - error: Operator "+" not supported for types "str" and "Literal[1]"
;;     Operator "+" not supported for types "str" and "Literal[1]" (reportGeneralTypeIssues)
;; 2 errors, 1 warning, 0 informations
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             ;; It would be nice if we could also capture the
             ;; \\(error\\|warning\\) part as "KIND", but I got messed
             ;; up on it
             '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'pyright)

;; Extra check commands for C-c C-v
(if (executable-find "mypy")
    (setq python-check-command "mypy"))
(if (executable-find "pyright")
    (setq python-check-command "pyright"))

;; I ran into something similar to this on Windows:
;; https://github.com/jorgenschaefer/elpy/issues/733
;;
;; The culprit was "App Execution Aliases" with python and python3
;; redirecting to the windows store. Using:
;;
;;     winkey -> Manage app execution aliases -> uncheck python and python3
;;
;; fixed it.

;; Also on Windows - a `pip install` of `pyreadline3' is required to
;; make tab-completion work at all. It provides the `readline' import
;; symbol.

;; Virtualenvs - require .dir-locals.el to have e.g.:
;;   ((python-mode . ((python-shell-virtualenv-root . "/path/to/my/.venv"))))
;; However, this only operates on `run-python' shells.
;;
;; `pyvenv' solves the otherwise very annoying problem of getting
;; external tools like `compile' and `eshell' to also use our virtual
;; environment's python.  I may still use .dir-locals.el to set things
;; like the python-check-command on a per-project basis, though.
(when (package-installed-p 'pyvenv)
  (pyvenv-mode)
  ;; (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
  (pyvenv-tracking-mode)
  ;; (setenv "WORKON_HOME" "~/.conda/envs")
  )

;; Enable semantic mode for more intelligent code parsing
;; https://www.gnu.org/software/emacs/manual/html_node/semantic/Semantic-mode.html
;; (add-hook 'python-mode-hook 'semantic-mode)

;; Don't mark the check command and virtualenv variables as unsafe
(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
