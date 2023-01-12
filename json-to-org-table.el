;;; json-to-org-table.el --- Converts json string to linked org table -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Joshua Person
;;
;; Author: Joshua Person <http://github.com/noonker>
;; Maintainer: Joshua Person <ceo@legitimate.company>
;; Created: December 06, 2020
;; Modified: December 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/noonker/json-to-org-table
;; Package-Requires: ((emacs 27.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Converts json string to linked org table
;;
;;; Code:
;;; TODO: Better Examples

(defvar j2t-debug nil)

(defvar j2t-cs-map [("\r" "")
                    ("\n" "")]
  "Map of characters to replace in json string.")

(defun j2t-cs (str)
  "Clean String.
Replace any string org mode wouldn't like according to the j2t-cs-map
STR: String to be replaced
RETURNS: A string with problematic characters returned"
  (seq-reduce
   (lambda (s m) (replace-regexp-in-string (car m) (cadr m) s))
   j2t-cs-map (format "%s" str)))

(defun j2t-lf (key &optional ref i)
  "Convert to link Link Format based on available args.
KEY: String or Symbol that becomes the name of the table
REF: If there is a Reference that becomes a subkey of the link
I: Is the Index for links in vectors"
  (cond (i (format "[[%s_%s%s]]" key ref (number-to-string i)))
        (ref (format "[[%s_%s]]" key ref))
        (t (format "[[%s]]" key))))

(defun j2t-hp (key value)
  "Hashmap Print prints a hashmap key-value table row.
KEY: Hashmap key column
VALUE: Hashmap value column"
  (format "|%s|%s|\n" (j2t-cs key) (j2t-cs value)))

(defmacro j2t-c+ (&rest str)
  "HACK: Concatenates all args and update the value of cur with new STR.
There's probably a better way to do this but this keeps things as clean
as possible in the =tablify= function."
  `(setq cur (concat cur (concat ,@str ))))

(defun j2t-parse-vector-vector (elt)
  "The row parser for a vector of vectors.
ELT: Is a vector to be turned into a table row
RETURNS: A table row representing the values of a vector"
  (let ((cur ""))
    (j2t-c+ "|")
    (mapc (lambda (x) (j2t-c+ (j2t-cs (format "%s" x)) "|" )) elt)
    (j2t-c+ "\n")
    cur
    )
  )

(defun j2t-parse-hashmap-vector (elt &optional ref i)
  "A row parser for a vector element composed of hashmaps.
ELT: A dotted pair cons representing a json hashmap
REF: Reference if this is a linked table
I: Optional Index for multiple linked tables
RETURNS: The table row representing values of a hashmap and a
         list of subtables to create if applicable
EXAMPLE: ((a . (b . 2)) (c . d) (e . f)) -> '(\"|[[a]]|d|f|]\" '(a (b .2) 1))"
  (let ((cur "")
        (keys (mapcar #'car elt))
        (nex '()))
    (mapcar (lambda (key)
            (let ((value (alist-get key elt)))
              (if (consp value)
                  (progn
                    (j2t-c+ (j2t-lf key ref i) "|")
                    (setq nex (append nex '('(key value i)))))
                (j2t-c+ (j2t-cs value) "|" )))
            ) keys)
    `(,cur ,nex)
    ))


  (defun j2t-parse-hash-element (elt &optional ref)
    "A row parser for elements of a hash map.
ELT: A dotted pair cons representing a json hashmap
REF: Reference if this is a linked table
RETURNS: Return an object who's first element is the generated string
         and the second element is the key if a new table is required.
EXAMPLE: (a . b) -> '(\"|a|b|\n\" '())"
    (let ((key (car elt))
          (val (cdr elt)))
      (cond ((not val) `(,(j2t-hp key "") nil))
            ((vectorp val) `(,(j2t-hp key (j2t-lf key ref)) ,key))
            ((consp val) `(,(j2t-hp key (j2t-lf key ref)) ,key))
            (t `(,(j2t-hp key (format "%s" val)) nil))
            )))

(defun j2t-tablify (elt &optional ref)
  "Function to be called recusrively to build an table.
ELT: a json object
REF: a reference is this is a linked table"
  (let ((cur "")
        (nex '()))
    (if j2t-debug (message (format "Got here! I was called with:\n  elt: %s\n  ref: %s\n" elt ref)))
    (if ref (j2t-c+ (format "#+name: %s\n" ref))) ;; If there's a reference add a name block to establish the linkage

    (cond
     ;; ----- Element is a hash-map -----
     ((consp elt)
      (progn
        (j2t-c+ "|key|value|\n|-\n") ;; Add headers for hashmap table
        ;; For each element in the hashmap either add the value or add a link to the table of values
        (mapc (lambda (x) (let ((parsed (j2t-parse-hash-element x ref)))
                       (format "x: %s\nparsed: %s" x parsed)
                       (j2t-c+ (car parsed))
                       (if (cadr parsed) (setq nex (append (cdr parsed) nex))))) elt)
        (j2t-c+ "\n")
        ;; Recursively call this function to create any subtables
        (mapc (lambda (x)  (progn  (if j2t-debug (message (format "\nThe symbol I'm going to look up is: %s\n  it's type is: %s\n  and the value is: %s" x (type-of x) (alist-get x elt))))
                              (if ref
                                  (j2t-c+ (j2t-tablify (alist-get x elt) (format "%s_%s" x ref)))
                                (j2t-c+ (j2t-tablify (alist-get x elt) (format "%s" x)))))) nex)
        ))

     ;; ----- Element is a vector and is a vector of hash-maps -----
     ((and (vectorp elt)
           (consp (aref elt 0)))
      (let ((keys (mapc #'car (aref elt 0)))
            )
        (j2t-c+ (format "|%s|\n" (string-join (mapcar (lambda (x) (format "%s" (car x))) keys) "|")))
        (j2t-c+ "|-\n")
        (seq-map-indexed
         (lambda (elt idx)
           (let ((parsed (j2t-parse-hashmap-vector elt ref idx)))
             (j2t-c+ "|")
             (j2t-c+ (car parsed))
             (j2t-c+ "\n")
             (if (cadr parsed) (setq nex (append (cdr parsed) nex))))
           ) elt)
        )

      ;; Recursively call this function to create any subtables
      (mapc (lambda (x) (let ((key (nth 0 x))
                         (value (nth 1 x))
                         (i (nth 2 x)))
                     (j2t-c+ (j2t-tablify value (format "%s_%s%s" key ref (format "%s" i)) )))) nex)
      )

     ;; ----- Element is a vector of vectors -----
     ((and (vectorp elt)
           (vectorp (aref elt 0)))
      (let ((a nil))
        (mapc (lambda (x) (j2t-c+ (j2t-parse-vector-vector x))) elt)
        (j2t-c+ "\n")
        ))

     ;; ----- Element is an empty vector -----
     ((and (vectorp elt)
           (= (length elt) 0))
      (j2t-c+ "| |\n")
      )

     ;; ----- Element is a vector of strings -----
     ((vectorp elt)
      (j2t-c+ (format "|%s|\n|-\n" ref))
      (mapc (lambda (x) (j2t-c+ "|" (j2t-cs x) "|" "\n")) elt)
      )
     )
    cur
    )
  )

(defun json-to-org-table-parse-json-string (str)
  "Read a json string, parse it, and return a tablified string.
STR: json string"
  (j2t-tablify (json-read-from-string str)))

(defun json-to-org-table-parse-json (js)
  "Read an Emacs json object, parse it, and return a tablified string.
The json should be in the format:
 - lists -> vectors
 - hashmaps -> alist cons
 - null -> \"\"
 - bool -> :json-true / :json-false
JS: json object"
  (j2t-tablify js))

(provide 'json-to-org-table)

;;; json-to-org-table.el ends here
