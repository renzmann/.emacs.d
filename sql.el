;; ============================================================================
;;                                SQL
;; ============================================================================
(require 'hive2)
(require 'ob-sql-mode)
(require 'sqlformat)
(require 'sqlup-mode)
(require 'sql-indent)

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
  ;; (setq indent-line-function 'renz/sql-indent)
  (setq tab-width 4)
  (setq sqlformat-command 'sql-formatter)
  (setq sqlind-basic-offset 4)
  ;; (setq electric-indent-inhibit t)
  )

(defvar renz/sql-indentation-offsets-alist
  `((select-clause 0)
    (insert-clause 0)
    (delete-clause 0)
    (update-clause 0)
    ,@sqlind-default-indentation-offsets-alist))

(add-hook 'sqlind-minor-mode-hook
    (lambda ()
       (setq sqlind-indentation-offsets-alist
             renz/sql-indentation-offsets-alist)))

(add-hook 'sql-mode-hook #'renz/sql-mode-hook)
(add-to-list 'auto-mode-alist '("\\.hql" . sql-mode))
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
