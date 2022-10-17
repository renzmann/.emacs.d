;; ============================================================================
;;                                SQL
;; ============================================================================
(setq-default indent-tabs-mode nil)
;; (setq electric-indent-inhibit t)

;; https://emacs.stackexchange.com/a/16513
(defun renz/sql-indent ()
  (let ((last-indent (if (> (line-number-at-pos) 1)
                         (save-excursion
                           (previous-line)
                           (back-to-indentation)
                           (current-column))
                       0)))
    (save-excursion
      (back-to-indentation)
      (if (and (eq last-command this-command)
               (> (point) (line-beginning-position)))
          (delete-region (max (line-beginning-position) (- (point) 4)) (point))
        (while (< (current-column) (+ 4 last-indent))
          (insert " "))))
    (if (< (point) (save-excursion (back-to-indentation) (point)))
        (back-to-indentation))))


(defun renz/sql-mode-hook ()
  (setq indent-line-function 'renz/sql-indent))

;; (add-hook 'sql-mode-hook 'renz/sql-mode-hook)
(require 'hive2)
(require 'ob-sql-mode)
(add-to-list 'auto-mode-alist '("\\.hql" . sql-mode))
(require 'sqlformat)
(setq sqlformat-command 'sql-formatter)
(require 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
