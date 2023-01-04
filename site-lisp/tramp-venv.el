;;; tramp-venv.el --- Hacky Tramp venv activation -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: python venv tramp
;; URL: https://robbmann.io/
;; Version: 0.1

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
;; Places a virtualenv's directory at the front of `tramp-remote-path'

;;; Code:

;; TODO: Need to fix a few things here:
;; 1. If not on Tramp, don't do anything
;; 2. Connection-specific virtual environments
;; 3. Check the venv if it's a valid environment
;; 4. Ensure we're pointing to ~bin~ directory correctly (don't add twice)

(defvar tramp-venv-active-venv nil)

(defun tramp-venv-activate (virtualenv)
  "Place `virtualenv' at the front of tramp-remote-path and cleanup the connection."
  (interactive "DEnv: ")
  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
         (venv-file-name (directory-file-name venv-dir))
         (bin-dir (directory-file-name (expand-file-name "bin" venv-file-name))))
    (setq tramp-venv-active-venv bin-dir)
    (add-to-list 'tramp-remote-path bin-dir)
    (tramp-cleanup-this-connection)
    (message (concat "Activated `" venv-dir "` on this Tramp connection"))))

(defun tramp-venv-deactivate ()
  "Deactivate the current virtual environment on this Tramp connection."
  (interactive)
  (let ((to-deactivate tramp-venv-active-venv))
    (setq tramp-remote-path (delete to-deactivate tramp-remote-path))
    (setq tramp-venv-active-venv nil)
    (tramp-cleanup-this-connection)
    (message (concat "Deactivated `" to-deactivate "'"))))

(provide 'tramp-venv)
