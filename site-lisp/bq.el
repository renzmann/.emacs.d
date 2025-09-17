;;; bq.el --- BigQuery SQL mode extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Robb Enzmann

;; Author: Robert Enzmann <robbenzmann@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((sql "3.0"))
;; Keywords: sql bigquery

;;; Commentary:

;; This package adds BQ to the sql-mode product list

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'sql)

(defcustom sql-bq-program "bq-shell"
  "Command to start the BQ shell."
  :type 'file
  :group 'SQL)

(defcustom sql-bq-options '()
  "List of additional options for `sql-bq-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-bq-login-params '()
  "List of login parameters needed to connect to BQ."
  :type 'sql-login-params
  :group 'SQL)

;;;###autoload
(defun sql-bq (&optional buffer)
  "Run BQ shell as an inferior process for BUFFER."
  (interactive "P")
  (sql-product-interactive 'bq buffer))

(sql-add-product
 'bq "BQ"
 :sqli-program 'sql-bq-program
 :sqli-options 'sql-bq-options
 :sqli-login 'sql-bq-login-params
 :sqli-comint-func 'sql-comint
 :prompt-regexp "^[a-zA-Z0-9\-]+> ")

(provide 'bq)
;;; bq.el ends here
