;;; corfu-popup.el --- Corfu popup on terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-04-11
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (corfu "0.23") (popon "0"))
;; Keywords: convenience
;; Homepage: https://codeberg.org/akib/emacs-corfu-terminal

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `corfu-popup' is now `corfu-terminal'.  This file is now obsolete and
;; only provided for backward compatibility, please install and use
;; corfu-terminal instead.

;;; Code:

(require 'corfu-terminal)

(display-warning
 'corfu-terminal
 "`corfu-popup' is now `corfu-terminal'.  This file (`corfu-popup') is now\
 obsolete and only provided for backward compatibility, please replace all\
 references of `corfu-popup' with `corfu-terminal'.")

(define-obsolete-variable-alias 'corfu-popup-position-right-margin
  'corfu-terminal-position-right-margin "0.1")

;;;###autoload
(define-obsolete-function-alias 'corfu-popup-mode 'corfu-terminal-mode
  "0.1")

(provide 'corfu-popup)
;;; corfu-popup.el ends here
