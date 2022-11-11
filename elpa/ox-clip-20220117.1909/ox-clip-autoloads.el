;;; ox-clip-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ox-clip" "ox-clip.el" (0 0 0 0))
;;; Generated autoloads from ox-clip.el

(autoload 'ox-clip-formatted-copy "ox-clip" "\
Export the selected region to HTML and copy it to the clipboard.
R1 and R2 define the selected region.

\(fn R1 R2)" t nil)

(autoload 'ox-clip-image-to-clipboard "ox-clip" "\
Copy the image file or latex fragment at point to the clipboard as an image.
SCALE is a numerical
prefix (default=`ox-clip-default-latex-scale') that determines
the size of the latex image. It has no effect on other kinds of
images. Currently only works on Linux.

\(fn &optional SCALE)" t nil)

(register-definition-prefixes "ox-clip" '("ox-clip-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ox-clip-autoloads.el ends here
