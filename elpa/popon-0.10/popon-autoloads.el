;;; popon-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "popon" "popon.el" (0 0 0 0))
;;; Generated autoloads from popon.el

(autoload 'poponp "popon" "\
Return t if OBJECT is a popon.

\(fn OBJECT)" nil nil)

(autoload 'popon-live-p "popon" "\
Return t if OBJECT is a popon and not killed.

\(fn OBJECT)" nil nil)

(autoload 'popon-get "popon" "\
Get the PROP property of popon POPON.

\(fn POPON PROP)" nil nil)

(autoload 'popon-put "popon" "\
Set the PROP property of popon POPON to VALUE.

\(fn POPON PROP VALUE)" nil nil)

(autoload 'popon-properties "popon" "\
Return a copy the property list of popon POPON.

\(fn POPON)" nil nil)

(autoload 'popon-position "popon" "\
Return the position of popon POPON as a cons (X, Y).

When popon POPON is killed, return nil.

\(fn POPON)" nil nil)

(autoload 'popon-size "popon" "\
Return the size of popon POPON as a cons (WIDTH . HEIGHT).

When popon POPON is killed, return nil.

\(fn POPON)" nil nil)

(autoload 'popon-window "popon" "\
Return the window popon POPON belongs to.

Return nil if popon POPON is killed.

\(fn POPON)" nil nil)

(autoload 'popon-buffer "popon" "\
Return the buffer popon POPON belongs to.

Return nil if popon POPON is killed.

\(fn POPON)" nil nil)

(autoload 'popon-text "popon" "\
Return the text popon POPON is displaying.

POPON may be a killed popon.  Return nil if POPON isn't a popon at
all.

\(fn POPON)" nil nil)

(autoload 'popon-create "popon" "\
Create a popon showing TEXT at POS of WINDOW.

Display popon only if WINDOW is displaying BUFFER.

POS is a cons (X, Y), where X is column and Y is line in WINDOW.  TEXT
should be a string or a cons cell of form (STR . WIDTH).  When TEXT is
a string, each line of it should be of same length (i.e `string-width'
should return the same length for every line).  When TEXT is a cons
cell, STR is used as the text to display and each line of it should be
of visual length width.

PRIORITY is a number (integer or float) between -100 and 100.  Popons
with larger priority values are rendered first.

\(fn TEXT POS &optional WINDOW BUFFER PRIORITY)" nil nil)

(autoload 'popon-kill "popon" "\
Kill popon POPON.

Do nothing if POPON isn't a live popon.  Return nil.

\(fn POPON)" nil nil)

(autoload 'popon-redisplay "popon" "\
Redisplay popon overlays." nil nil)

(autoload 'popon-update "popon" "\
Update popons if needed." nil nil)

(autoload 'popon-x-y-at-pos "popon" "\
Return the (X, Y) coodinate of POINT in selected window as a cons.

Return nil if POINT is not in visible text area.

NOTE: This uses `posn-at-point', which is slow.  So try to minimize
calls to this function.

\(fn POINT)" nil nil)

(autoload 'popon-kill-all "popon" "\
Kill all popons." t nil)

(register-definition-prefixes "popon" '("popon-"))

;;;***

;;;### (autoloads nil nil ("popon-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; popon-autoloads.el ends here
