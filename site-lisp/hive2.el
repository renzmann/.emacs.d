;;; hive.el --- Hive SQL mode extension

;; Copyright (C) 2022 Robb Enzmann

;; Author: Robert Enzmann <robbenzmann@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((sql "3.0"))
;; Keywords: sql hive hsql

;;; Commentary:

;; This package adds Hive to the sql-mode product list.

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

(require 'sql)

(defcustom sql-hive-program "hive2"
  "Command to start the Hive client."
  :type 'file
  :group 'SQL)

(defcustom sql-hive-options '()
  "List of additional options for `sql-hive-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-hive-login-params '()
  "List of login parameters needed to connect to Hive."
  :type 'sql-login-params
  :group 'SQL)

;;;###autoload
(defun sql-hive (&optional buffer)
  "Run hive as an inferior process."
  (interactive "P")
  (sql-product-interactive 'hive buffer))

(sql-add-product
 'hive2 "Hive"
 :sqli-program 'sql-hive-program
 :sqli-options 'sql-hive-options
 :sqli-login 'sql-hive-login-params
 :sqli-comint-func 'sql-comint
 :prompt-regexp "^[0-9]+:[[:blank:]]+jdbc:hive2://[a-zA-Z0-9.]+:[0-9]+,[a-zA-Z0-9]+>[[:blank:]]+"
 :prompt-cont-regexp "^[0-9]+:[[:blank:]]+jdbc:hive2://[a-zA-Z0-9.]+:[0-9]+,[a-zA-Z0-9]+>[[:blank:]]+"
 )

(provide 'hive2)

;;; hive2.el ends here
