;;; pyrightconfig.el --- Hacky pyright venv configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: python pyright venv tramp
;; URL: https://robbmann.io/
;; Version: 0.1
;; Package-Requires: ((f "0.17.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Sets `pyrightconfig.json' in the git root directory.  Useful for using eglot + tramp + virtualenv + python

;;; Code:

(require 'f)

(defun pyrightconfig--json-contents (venvPath venv)
  (format "{
    \"venvPath\": \"%s\",
    \"venv\": \"%s\"
}" venvPath venv))

(defun pyrightconfig-write (virtualenv)
  (interactive "DEnv: ")
  ;; Naming convention for venvPath matches the fields for pyrightconfig.json
  (let* ((standard-file-name (convert-standard-filename virtualenv))
         (venvPath (f-parent standard-file-name))
         (venv (f-base standard-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (pyrightconfig--json-contents venvPath venv)))
    (f-write-text out-contents 'utf-8 out-file)))

(provide 'pyrightconfig)

;;; pyrightconfig.el ends here
