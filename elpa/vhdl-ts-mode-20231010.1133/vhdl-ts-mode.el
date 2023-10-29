;;; vhdl-ts-mode.el --- VHDL Tree-sitter major mode -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/vhdl-ts-mode
;; Version: 0.1.0
;; Keywords: VHDL, IDE, Tools
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode to navigate and edit VHDL files with tree-sitter.
;;
;; Provides tree-sitter based implementations for the following features:
;; - Syntax highlighting
;; - Indentation
;; - `imenu'
;; - `which-func'
;; - Navigation functions
;; - Completion at point
;;
;;
;; Contributions:
;;   This major mode is still under active development!
;;   Check contributing guidelines:
;;     - https://github.com/gmlarumbe/vhdl-ext#contributing
;;
;;   For some highlight queries examples, check the link below:
;;    - https://github.com/alemuller/tree-sitter-vhdl/blob/main/queries/highlights.scm

;;; Code:

;;; Requirements
(require 'treesit)
(require 'vhdl-mode)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")


;;; Customization
(defgroup vhdl-ts nil
  "VHDL tree-sitter mode."
  :group 'languages)

(defcustom vhdl-ts-indent-level 4
  "Tree-sitter indentation of VHDL statements with respect to containing block."
  :group 'vhdl-ts
  :type 'integer)

(defcustom vhdl-ts-file-extension-re "\\.vhdl?\\'"
  "VHDL file extensions.
Defaults to .vhd and .vhdl."
  :group 'vhdl-ts
  :type 'string)

;;; Utils
;;;; Core
(defconst vhdl-ts-identifier-re "\\(identifier\\|simple_name\\)")
(defconst vhdl-ts-instance-re "\\_<component_instantiation_statement\\_>")

(defun vhdl-ts--node-at-point ()
  "Return tree-sitter node at point."
  (treesit-node-at (point) 'vhdl))

(defun vhdl-ts--node-has-parent-recursive (node node-type)
  "Return non-nil if NODE is part of NODE-TYPE in the parsed tree."
  (treesit-parent-until
   node
   (lambda (node) ; Third argument must be a function
     (string-match node-type (treesit-node-type node)))))

(defun vhdl-ts--node-has-child-recursive (node node-type)
  "Return first node of type NODE-TYPE that is a child of NODE in the parsed tree.
If none is found, return nil."
  (treesit-search-subtree node node-type))

(defun vhdl-ts--node-identifier-name (node)
  "Return identifier name of NODE."
  (let (temp-node)
    (when node
      (cond ((string-match vhdl-ts-instance-re (treesit-node-type node))
             (cond ((setq temp-node (treesit-search-subtree node "\\_<component_instantiation\\_>"))
                    (treesit-node-text (treesit-node-child-by-field-name temp-node "component") :no-prop))
                   ((setq temp-node (treesit-search-subtree node "entity_instantiation"))
                    (treesit-search-subtree node "entity_instantiation")
                    (treesit-node-text (treesit-node-child-by-field-name (treesit-node-child temp-node 1) "suffix") :no-props))
                   (t (error "Unexpected component_instantiation_statement subnode!"))))
            (t
             (treesit-node-text (treesit-search-subtree node vhdl-ts-identifier-re) :no-prop))))))

(defun vhdl-ts--node-instance-name (node)
  "Return identifier name of NODE.

Node must be of type `vhdl-ts-instance-re'.  Otherwise return nil."
  (unless (and node
               (string-match vhdl-ts-instance-re (treesit-node-type node)))
    (error "Wrong node type: %s" (treesit-node-type node)))
  (treesit-node-text (treesit-search-subtree node "identifier") :no-props))

(defun vhdl-ts--highest-node-at-pos (pos)
  "Return highest node starting at POS in the parsed tree.

Only might work as expected if point is at the beginning of a symbol.

Snippet fetched from `treesit--indent-1'."
  (let* ((smallest-node (vhdl-ts--node-at-point))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq pos (treesit-node-start node))))))
    node))

(defun vhdl-ts--highest-node-at-symbol ()
  "Return highest node in the hierarchy for symbol at point.
Check also `treesit-thing-at-point' for similar functionality."
  (vhdl-ts--highest-node-at-pos (car (bounds-of-thing-at-point 'symbol))))

(defun vhdl-ts--node-at-bol ()
  "Return node at first non-blank character of current line.
Snippet fetched from `treesit--indent-1'."
  (let* ((bol (save-excursion
                (forward-line 0)
                (skip-chars-forward " \t")
                (point)))
         (smallest-node
          (cond ((null (treesit-parser-list)) nil)
                ((eq 1 (length (treesit-parser-list)))
                 (treesit-node-at bol))
                ((treesit-language-at (point))
                 (treesit-node-at bol (treesit-language-at (point))))
                (t (treesit-node-at bol))))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq bol (treesit-node-start node))))))
    node))

(defun vhdl-ts-nodes (pred &optional start)
  "Return current buffer NODES that match PRED.

If optional arg START is non-nil, use it as the initial node to search in the
tree."
  (let ((root-node (or start (treesit-buffer-root-node))))
    (mapcar #'car (cdr (treesit-induce-sparse-tree root-node pred)))))

(defun vhdl-ts-nodes-props (pred &optional start)
  "Return nodes and properties that satisfy PRED in current buffer.

If optional arg START is non-nil, use it as the initial node to search in the
tree.

Returned properties are a property list that include node name, start position
and end position."
  (mapcar (lambda (node)
            `(,node :name ,(vhdl-ts--node-identifier-name node)
                    :start-pos ,(treesit-node-start node)
                    :end-pos ,(treesit-node-end node)))
          (vhdl-ts-nodes pred start)))

;;;; Context
(defconst vhdl-ts-block-at-point-re
  (eval-when-compile
    (regexp-opt
     '("entity_declaration"
       "architecture_body"
       "process_statement"
       "procedure_body"
       "function_body"
       "generate_statement_body"
       "component_instantiation_statement"))))

(defun vhdl-ts-entity-at-point ()
  "Return node of entity at point."
  (vhdl-ts--node-has-parent-recursive (vhdl-ts--node-at-point) "entity_declaration"))

(defun vhdl-ts-arch-at-point ()
  "Return node of architectre body at point."
  (vhdl-ts--node-has-parent-recursive (vhdl-ts--node-at-point) "architecture_body"))

(defun vhdl-ts-instance-at-point ()
  "Return node of instance at point."
  (vhdl-ts--node-has-parent-recursive (vhdl-ts--node-at-point) vhdl-ts-instance-re))

(defun vhdl-ts-block-at-point ()
  "Return node of block at point."
  (vhdl-ts--node-has-parent-recursive (vhdl-ts--node-at-point) vhdl-ts-block-at-point-re))

(defun vhdl-ts-nodes-block-at-point (pred)
  "Return block at point NODES that match PRED."
  (mapcar #'car (cdr (treesit-induce-sparse-tree (vhdl-ts-block-at-point) pred))))

(defun vhdl-ts-search-node-block-at-point (pred &optional backward all)
  "Search forward for node matching PRED inside block at point.

By default, only search for named nodes, but if ALL is non-nil, search
for all nodes.  If BACKWARD is non-nil, search backwards."
  (treesit-search-forward (vhdl-ts--node-at-point)
                          (lambda (node)
                            (and (string-match pred (treesit-node-type node))
                                 (< (treesit-node-end node) (treesit-node-end (vhdl-ts-block-at-point)))))
                          backward
                          all))

;; Some examples using previous API
(defun vhdl-ts-entity-declarations-nodes-current-buffer ()
  "Return entity declaration nodes of current file."
  (vhdl-ts-nodes "entity_declaration"))

(defun vhdl-ts-entity-declarations-current-buffer ()
  "Return entity declaration names of current file."
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props "entity_declaration")))

(defun vhdl-ts-arch-body-nodes-current-buffer ()
  "Return architecture body nodes of current file."
  (vhdl-ts-nodes "architecture_body"))

(defun vhdl-ts-arch-body-current-buffer ()
  "Return architecture body names of current file."
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props "architecture_body")))

(defun vhdl-ts-arch-instances-nodes (arch-node)
  "Return instance nodes of ARCH-NODE."
  (unless (and arch-node (string= "architecture_body" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (vhdl-ts-nodes vhdl-ts-instance-re arch-node))

(defun vhdl-ts-arch-instances (arch-node)
  "Return instances of ARCH-NODE."
  (unless (and arch-node (string= "architecture_body" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props vhdl-ts-instance-re arch-node)))

(defun vhdl-ts-arch-process-blocks (arch-node)
  "Return process blocks of ARCH-NODE."
  (unless (and arch-node (string= "architecture_body" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props "process_statement" arch-node)))

(defun vhdl-ts-arch-concurrent-assignments (arch-node)
  "Return concurrent assignments of ARCH-NODE."
  (unless (and arch-node (string= "architecture_body" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props "simple_concurrent_signal_assignment" arch-node)))

(defun vhdl-ts-arch-entity-name (arch-node)
  "Return associated entity name of ARCH-NODE."
  (unless (and arch-node (string= "architecture_body" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (treesit-node-text (treesit-node-child-by-field-name arch-node "entity") :no-props))

;;;; Navigation
(defun vhdl-ts-forward-sexp (&optional arg)
  "Move forward across S-expressions.

With `prefix-arg', move ARG expressions."
  (interactive "p")
  (if (member (following-char) '(?\( ?\{ ?\[))
      (if (and arg (< arg 0))
          (backward-sexp arg)
        (forward-sexp arg))
    (let* ((node (vhdl-ts--highest-node-at-symbol))
           (beg (treesit-node-start node))
           (end (treesit-node-end node)))
      (if (and arg (< arg 0))
          (goto-char beg)
        (goto-char end)))))

(defun vhdl-ts-backward-sexp (&optional arg)
  "Move backward across S-expressions.

With `prefix-arg', move ARG expressions."
  (interactive "p")
  (if (member (preceding-char) '(?\) ?\} ?\]))
      (if (and arg (< arg 0))
          (forward-sexp arg)
        (backward-sexp arg))
    (let* ((node (treesit-node-parent (vhdl-ts--node-at-point)))
           (beg (treesit-node-start node))
           (end (treesit-node-end node)))
      (if (and arg (< arg 0))
          (goto-char end)
        (goto-char beg)))))


;;; Font-lock
;;;; Faces
(defgroup vhdl-ts-faces nil
  "VHDL-ts faces."
  :group 'vhdl-ts)

(defvar vhdl-ts-font-lock-then-face 'vhdl-ts-font-lock-then-face)
(defface vhdl-ts-font-lock-then-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for if-else grouping keyword: then."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-punctuation-face 'vhdl-ts-font-lock-punctuation-face)
(defface vhdl-ts-font-lock-punctuation-face
  '((t (:inherit font-lock-punctuation-face)))
  "Face for punctuation symbols:
!,;:?'=<>*"
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-operator-face 'vhdl-ts-font-lock-operator-face)
(defface vhdl-ts-font-lock-operator-face
  '((t (:inherit font-lock-operator-face)))
  "Face for operator symbols, such as &^~+-/|."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-parenthesis-face 'vhdl-ts-font-lock-parenthesis-face)
(defface vhdl-ts-font-lock-parenthesis-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for parenthesis ()."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-brackets-content-face 'vhdl-ts-font-lock-brackets-content-face)
(defface vhdl-ts-font-lock-brackets-content-face
  '((t (:inherit font-lock-number-face)))
  "Face for content between brackets: arrays, bit vector width and indexing."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-port-connection-face 'vhdl-ts-font-lock-port-connection-face)
(defface vhdl-ts-font-lock-port-connection-face
  '((t (:inherit font-lock-constant-face)))
  "Face for port connections of instances.
portA => signalA,
portB => signalB
);"
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-entity-face 'vhdl-ts-font-lock-entity-face)
(defface vhdl-ts-font-lock-entity-face
  '((t (:inherit font-lock-function-call-face)))
  "Face for entity names."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-instance-face 'vhdl-ts-font-lock-instance-face)
(defface vhdl-ts-font-lock-instance-face
  '((t (:inherit font-lock-variable-use-face)))
  "Face for instance names."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-instance-lib-face 'vhdl-ts-font-lock-instance-lib-face)
(defface vhdl-ts-font-lock-instance-lib-face
  '((t (:inherit font-lock-property-name-face)))
  "Face for instances lib prefix."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-translate-off-face 'vhdl-ts-font-lock-translate-off-face)
(defface vhdl-ts-font-lock-translate-off-face
  '((t (:slant italic)))
  "Face for pragmas between comments, e.g:
* translate_off / * translate_on"
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-error-face 'vhdl-ts-font-lock-error-face)
(defface vhdl-ts-font-lock-error-face
  '((t (:underline (:style wave :color "Red1"))))
  "Face for tree-sitter parsing errors."
  :group 'vhdl-ts-faces)


;;;; Keywords
(defconst vhdl-ts-keywords (append vhdl-02-keywords vhdl-08-keywords))
(defconst vhdl-ts-types (append vhdl-02-types vhdl-08-types vhdl-math-types))
(defconst vhdl-ts-types-regexp (concat "\\<\\(" (regexp-opt vhdl-ts-types) "\\)\\>"))
(defconst vhdl-ts-attributes (append vhdl-02-attributes vhdl-08-attributes))
(defconst vhdl-ts-enum-values vhdl-02-enum-values)
(defconst vhdl-ts-constants vhdl-math-constants)
(defconst vhdl-ts-functions (append vhdl-02-functions vhdl-08-functions vhdl-math-functions))
(defconst vhdl-ts-functions-regexp (concat "\\<\\(" (regexp-opt vhdl-ts-functions) "\\)\\>"))
(defconst vhdl-ts-packages (append vhdl-02-packages vhdl-08-packages vhdl-math-packages))
(defconst vhdl-ts-directives vhdl-08-directives)
(defconst vhdl-ts-operators-relational '("=" "/=" "<" ">"
                                         "<=" ; Less or equal/signal assignment
                                         "=>" ; Greater or equal/port connection
                                         ":=")) ; Not an operator, but falls better here
(defconst vhdl-ts-operators-arithmetic '("+" "-" "*" "/" "**" "&"))
(defconst vhdl-ts-punctuation '(";" ":" "," "'" "|" "." "!" "?"))
(defconst vhdl-ts-parenthesis '("(" ")" "[" "]"))

;;;; Treesit-settings
(defvar vhdl-ts--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'vhdl
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'vhdl
   '(((string_literal) @font-lock-string-face)
     ((bit_string_literal) @font-lock-string-face)
     ((character_literal) @font-lock-string-face))

   :feature 'error
   :language 'vhdl
   '((ERROR) @vhdl-ts-font-lock-error-face)

   ;; Place before 'keywords to override things like "downto" in ranges
   :feature 'all
   :language 'vhdl
   '(;; Library
     (library_clause
      (logical_name_list (simple_name) @font-lock-builtin-face))
     (use_clause
      (selected_name
       (selected_name (simple_name) @font-lock-function-name-face)))
     ;; Package
     (package_declaration
      (identifier) @font-lock-function-name-face)
     (package_body
      (simple_name) @font-lock-function-name-face)
     ;; Entity
     (entity_declaration
      name: (identifier) @font-lock-function-name-face)
     (entity_declaration
      at_end: (simple_name) @font-lock-function-name-face)
     ;; Architecture
     (architecture_body
      (identifier) @font-lock-function-name-face
      (simple_name) @font-lock-function-name-face)
     ;; Component
     (component_declaration
      name: (identifier) @font-lock-function-name-face)
     ;; Generate
     (if_generate_statement
      (label (identifier) @font-lock-constant-face))
     (for_generate_statement
      (label (identifier) @font-lock-constant-face))
     ;; Process
     (process_statement
      (label (identifier) @font-lock-constant-face))
     (process_statement
      (sensitivity_list (simple_name) @font-lock-constant-face))
     ;; Instances
     (component_instantiation_statement
      (label
       (identifier) @vhdl-ts-font-lock-instance-face)
      (entity_instantiation
       (selected_name
        prefix: (simple_name) @vhdl-ts-font-lock-instance-lib-face
        suffix: (simple_name) @vhdl-ts-font-lock-entity-face)))
     (component_instantiation_statement
      (label (identifier) @vhdl-ts-font-lock-instance-face)
      (component_instantiation (simple_name) @vhdl-ts-font-lock-entity-face))
     (component_instantiation_statement
      (label (identifier) @vhdl-ts-font-lock-instance-face)
      (entity_instantiation (simple_name) @vhdl-ts-font-lock-entity-face))
     ;; Port connections
     (association_list
      (named_association_element
       formal_part: (simple_name) @vhdl-ts-font-lock-port-connection-face))
     (association_list
      (named_association_element
       formal_part: (selected_name
                     prefix: (simple_name) @vhdl-ts-font-lock-instance-lib-face
                     suffix: (simple_name) @vhdl-ts-font-lock-port-connection-face)))
     (association_list
      (named_association_element
       formal_part:
       (ambiguous_name
        (simple_name) @vhdl-ts-font-lock-port-connection-face)))
     (association_list
      (named_association_element
       formal_part:
       (slice_name
        (simple_name) @vhdl-ts-font-lock-port-connection-face)))
     ;; Ranges
     (descending_range
      high: (simple_expression) @vhdl-ts-font-lock-brackets-content-face)
     (descending_range
      low: (simple_expression) @vhdl-ts-font-lock-brackets-content-face)
     (ascending_range
      high: (simple_expression) @vhdl-ts-font-lock-brackets-content-face)
     (ascending_range
      low: (simple_expression) @vhdl-ts-font-lock-brackets-content-face)
     (expression_list
      (expression (integer_decimal) @vhdl-ts-font-lock-brackets-content-face))
     (expression_list
      (expression (simple_name) @vhdl-ts-font-lock-brackets-content-face))
     (["downto" "to"] @vhdl-ts-font-lock-instance-lib-face)
     ;; Constants
     (constant_declaration
      (identifier_list (identifier) @font-lock-constant-face))
     ;; Alias
     (alias_declaration
      designator : (identifier) @font-lock-constant-face)
     ;; Enum labels
     (enumeration_type_definition
      literal: (identifier) @font-lock-constant-face)
     ;; Record members
     (selected_name
      prefix: (simple_name) @vhdl-ts-font-lock-instance-lib-face)
     ;; clk'event
     (attribute_name
      prefix: (simple_name) @font-lock-builtin-face
      (predefined_designator) @font-lock-builtin-face))

   :feature 'keyword
   :language 'vhdl
   `((["then"] @vhdl-ts-font-lock-then-face)
     ([,@vhdl-ts-keywords] @font-lock-keyword-face))

   :feature 'operator
   :language 'vhdl
   `(([,@vhdl-ts-operators-relational] @vhdl-ts-font-lock-punctuation-face)
     ([,@vhdl-ts-operators-arithmetic] @vhdl-ts-font-lock-operator-face))

   :feature 'punctuation
   :language 'vhdl
   `(([,@vhdl-ts-punctuation] @vhdl-ts-font-lock-punctuation-face)
     ([,@vhdl-ts-parenthesis] @vhdl-ts-font-lock-parenthesis-face))

   :feature 'types
   :language 'vhdl
   `((full_type_declaration
      name: (identifier) @font-lock-type-face)
     (subtype_declaration
      name: (identifier) @font-lock-type-face)
     ((ambiguous_name
       prefix: (simple_name) @font-lock-type-face)
      (:match ,vhdl-ts-types-regexp @font-lock-type-face))
     (subtype_indication
      (type_mark
       (simple_name) @font-lock-type-face)))

   :feature 'function
   :language 'vhdl
   '((procedure_declaration (identifier) @font-lock-function-name-face)
     (procedure_body (identifier) @font-lock-function-name-face)
     (function_declaration (identifier) @font-lock-function-name-face)
     (function_body (identifier) @font-lock-function-name-face)
     ;; Overloading
     (function_declaration (operator_symbol) @font-lock-function-name-face)
     (function_body (operator_symbol) @font-lock-function-name-face))

   :feature 'builtin
   :language 'vhdl
   `(((ambiguous_name
       prefix: (simple_name) @font-lock-builtin-face)
      (:match ,vhdl-ts-functions-regexp @font-lock-builtin-face)))))


;;; Indent
;;;; Matchers
(defun vhdl-ts--matcher-blank-line (&rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at a blank line."
  (let ((node (vhdl-ts--node-at-bol)))
    (unless node
      t)))

(defun vhdl-ts--matcher-generic-or-port (&rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at generic/port declaration."
  (let* ((node (vhdl-ts--node-at-bol))
         (node-type (treesit-node-type node))
         (entity-or-comp-node (vhdl-ts--node-has-parent-recursive node "\\(entity\\|component\\)_declaration")))
    (when (and entity-or-comp-node
               (or (string= "generic_clause" node-type)
                   (string= "port_clause" node-type)
                   (string= "entity_header" node-type)
                   (string= "component_header" node-type)))
      (treesit-node-start entity-or-comp-node))))

(defun vhdl-ts--matcher-keyword (&rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at a VHDL keyword, somehow as a fallback."
  (let ((node (vhdl-ts--node-at-bol)))
    (member (treesit-node-type node) vhdl-ts-keywords)))

(defun vhdl-ts--matcher-punctuation (&rest _)
  "A tree-sitter simple indent matcher.
Matches if point is at a punctuation/operator char, somehow as a fallback."
  (let ((node (vhdl-ts--node-at-bol)))
    (member (treesit-node-type node) `(,@vhdl-ts-punctuation ,@vhdl-ts-parenthesis))))

(defun vhdl-ts--matcher-default (&rest _)
  "A tree-sitter simple indent matcher."
  t)


;;;; Anchors
(defun vhdl-ts--anchor-point-min (&rest _)
  "A tree-sitter simple indent anchor."
  (save-excursion
    (point-min)))

(defun vhdl-ts--anchor-instance-port (node &rest _)
  "A tree-sitter simple indent anchor for NODE."
  (let ((label-node (vhdl-ts--node-has-parent-recursive node "\\(component_instantiation\\|call\\)_statement")))
    (treesit-node-start label-node)))

(defun vhdl-ts--anchor-concurrent-signal-assignment (node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let ((gen-node (vhdl-ts--node-has-parent-recursive node "\\(for\\|if\\)_generate_statement")))
    (if gen-node
        (treesit-node-start gen-node)
      (treesit-node-start (treesit-node-parent parent)))))


;;;; Rules
(defconst vhdl-ts--indent-zero-parent-node-re
  (eval-when-compile
    (regexp-opt '("design_file" "context_clause" "design_unit"))))

;; INFO: Do not use siblings as anchors, since comments could be wrongly detected as siblings!
(defvar vhdl-ts--indent-rules
  `((vhdl
     ;; Comments
     ((and (node-is "comment") (parent-is ,vhdl-ts--indent-zero-parent-node-re)) parent-bol 0)
     ((node-is "comment") grand-parent ,vhdl-ts-indent-level)
     ;; Zero-indent
     ((node-is "library_clause") parent-bol 0)
     ((node-is "use_clause") parent-bol 0)
     ((node-is "design_unit") parent-bol 0) ; architecture_body
     ((node-is "entity_declaration") parent-bol 0)
     ((node-is "architecture_body") parent-bol 0)
     ((node-is "package_declaration") parent-bol 0)
     ((node-is "package_body") parent-bol 0)
     ;; Generics and ports
     (vhdl-ts--matcher-generic-or-port grand-parent ,vhdl-ts-indent-level)
     ((node-is "constant_interface_declaration") parent-bol ,vhdl-ts-indent-level) ; Generic ports
     ((node-is "signal_interface_declaration") parent-bol ,vhdl-ts-indent-level) ; Signal ports
     ;; Declarations
     ((node-is "declarative_part") parent-bol ,vhdl-ts-indent-level) ; First declaration of the declarative part
     ((node-is "component_declaration") grand-parent ,vhdl-ts-indent-level)
     ((node-is "signal_declaration") grand-parent ,vhdl-ts-indent-level)
     ((node-is "constant_declaration") grand-parent ,vhdl-ts-indent-level)
     ((node-is "full_type_declaration") grand-parent ,vhdl-ts-indent-level)
     ((node-is "element_declaration") grand-parent ,vhdl-ts-indent-level)
     ((node-is "variable_declaration") grand-parent ,vhdl-ts-indent-level)
     ((node-is "procedure_declaration") grand-parent ,vhdl-ts-indent-level)
     ((node-is "function_declaration") grand-parent ,vhdl-ts-indent-level)
     ((node-is "function_body") grand-parent ,vhdl-ts-indent-level)
     ((node-is "procedure_body") grand-parent ,vhdl-ts-indent-level)
     ;; Concurrent & generate
     ((node-is "concurrent_statement_part") parent-bol ,vhdl-ts-indent-level) ; First signal declaration of a declarative part
     ((node-is "generate_statement_body") parent-bol ,vhdl-ts-indent-level)
     ((node-is "for_generate_statement") grand-parent ,vhdl-ts-indent-level)
     ((node-is "if_generate_statement") grand-parent ,vhdl-ts-indent-level)
     ((node-is "simple_concurrent_signal_assignment") vhdl-ts--anchor-concurrent-signal-assignment ,vhdl-ts-indent-level) ; Parent is (concurrent_statement_part)
     ((node-is "conditional_concurrent_signal_assignment") vhdl-ts--anchor-concurrent-signal-assignment ,vhdl-ts-indent-level)
     ((node-is "alternative_conditional_waveforms") parent-bol ,vhdl-ts-indent-level) ; Each of the else clases in conditional concurrent assignment
     ((node-is "process_statement") grand-parent ,vhdl-ts-indent-level) ; Grandparent is architecture_body
     ;; Instances
     ((node-is "component_instantiation_statement") grand-parent ,vhdl-ts-indent-level)
     ((node-is "component_map_aspect") grand-parent 0) ; Generic map if present, otherwise port map
     ((node-is "port_map_aspect") parent-bol 0) ; Port map only when there are generics
     ((node-is "association_list") vhdl-ts--anchor-instance-port ,vhdl-ts-indent-level)
     ((node-is "named_association_element") parent-bol 0)
     ;; Procedural
     ((node-is "sequence_of_statements") parent-bol ,vhdl-ts-indent-level) ; Statements inside process
     ((parent-is "sequence_of_statements") grand-parent ,vhdl-ts-indent-level)
     ((or (node-is "if") (node-is "else") (node-is "elsif")) parent-bol 0)
     ((node-is "case_statement") grand-parent ,vhdl-ts-indent-level)
     ((node-is "case_statement_alternative") parent-bol ,vhdl-ts-indent-level)
     ;; Others
     ((node-is "aggregate") grand-parent ,vhdl-ts-indent-level) ; Aggregates/array elements
     ((node-is "positional_element_association") parent-bol 0)  ; Check test/files/indent/tree-sitter/indent_misc.vhd:42
     ;; Opening & closing
     ((node-is "begin") parent-bol 0)
     ((node-is "end") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ;; Fallbacks/default
     ((and vhdl-ts--matcher-blank-line (parent-is ,vhdl-ts--indent-zero-parent-node-re)) parent-bol 0)
     (vhdl-ts--matcher-blank-line parent-bol ,vhdl-ts-indent-level) ; Blank lines
     ((or vhdl-ts--matcher-keyword vhdl-ts--matcher-punctuation) parent-bol ,vhdl-ts-indent-level)
     (vhdl-ts--matcher-default parent 0))))


;;; Imenu
(defconst vhdl-ts-imenu-create-index-re
  (eval-when-compile
    (regexp-opt
     '("entity_declaration"
       "architecture_body"
       "process_statement"
       "procedure_body"
       "function_body"
       "generate_statement_body"
       "component_instantiation_statement"))))

(defvar vhdl-ts-imenu-format-item-label-function
  'vhdl-ts-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar vhdl-ts-imenu-format-parent-item-label-function
  'vhdl-ts-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar vhdl-ts-imenu-format-parent-item-jump-label-function
  'vhdl-ts-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun vhdl-ts-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun vhdl-ts-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (vhdl-ts-imenu-format-item-label type name)))

(defun vhdl-ts-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (cond ((eq type 'ent)
         "*entity declaration*")
        ((eq type 'arch)
         "*architecture declaration*")
        ((eq type 'fun)
         "*function definition*")
        ((eq type 'pcd)
         "*procedure body*")
        (t
         (format "*%s*" type))))

(defun vhdl-ts-imenu-treesit-create-index-1 (node)
  "Given a sparse tree, create an imenu alist.

NODE is the root node of the tree returned by
`treesit-induce-sparse-tree' (not a tree-sitter node, its car is
a tree-sitter node).  Walk that tree and return an imenu alist.

Return a list of ENTRY where

ENTRY := (NAME . MARKER)
       | (NAME . ((JUMP-LABEL . MARKER)
                  ENTRY
                  ...)

NAME is the function/class's name, JUMP-LABEL is like \"*function
definition*\".

Copied from Python's `python--imenu-treesit-create-index-1' and adapted to
VHDL parser."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'vhdl-ts-imenu-treesit-create-index-1
                           children))
         (type (pcase (treesit-node-type ts-node)
                 ("entity_declaration"                'ent)
                 ("architecture_body"                 'arch)
                 ("process_statement"                 'proc)
                 ("procedure_body"                    'pcd)
                 ("function_body"                     'fun)
                 ("generate_statement_body"           'gen)
                 ("component_instantiation_statement" 'cmp)))
         ;; The root of the tree could have a nil ts-node.
         (name (when ts-node
                 (or (vhdl-ts--node-identifier-name ts-node)
                     "Anonymous")))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((null ts-node)
      subtrees)
     (subtrees
      (let ((parent-label (funcall vhdl-ts-imenu-format-parent-item-label-function
                                   type
                                   name))
            (jump-label (funcall vhdl-ts-imenu-format-parent-item-jump-label-function
                                 type
                                 name)))
        `((,parent-label
           ,(cons jump-label marker)
           ,@subtrees))))
     (t (let ((label (funcall vhdl-ts-imenu-format-item-label-function
                              type
                              name)))
          (list (cons label marker)))))))

(defun vhdl-ts-imenu-create-index (&optional node)
  "Imenu index builder function for `vhdl-ts-mode'.

NODE is the root node of the subtree you want to build an index
of.  If nil, use the root node of the whole parse tree.

Copied from Python's `python-imenu-treesit-create-index' and adapted
to VHDL parser."
  (let* ((node (or node (treesit-buffer-root-node 'vhdl)))
         (tree (treesit-induce-sparse-tree
                node
                vhdl-ts-imenu-create-index-re
                nil 1000)))
    (vhdl-ts-imenu-treesit-create-index-1 tree)))

(defun vhdl-ts--defun-name (node)
  "Return the defun name of NODE.

Return nil if there is no name or if NODE is not a defun node."
  (vhdl-ts--node-identifier-name node))


;;; Which-func
(defvar-local vhdl-ts-which-func-extra nil
  "Variable to hold extra information for `which-func'.")

(defun vhdl-ts-which-func-shorten-block (block-type)
  "Return shortened name of BLOCK-TYPE, if possible."
  (cond ((string= "entity_declaration"                block-type) "ent")
        ((string= "architecture_body"                 block-type) "arch")
        ((string= "process_statement"                 block-type) "proc")
        ((string= "procedure_body"                    block-type) "pcd")
        ((string= "function_body"                     block-type) "fun")
        ((string= "generate_statement_body"           block-type) "gen")
        ((string= "component_instantiation_statement" block-type) "cmp")
        (t block-type)))

(defun vhdl-ts-which-func-function ()
  "Retrieve `which-func' candidates."
  (let ((node (vhdl-ts--node-has-parent-recursive (vhdl-ts--node-at-point) vhdl-ts-imenu-create-index-re)))
    (when node
      (setq vhdl-ts-which-func-extra (vhdl-ts--node-identifier-name node))
      (vhdl-ts-which-func-shorten-block (treesit-node-type node)))))

(defun vhdl-ts-which-func ()
  "Hook for `vhdl-ts-mode' to enable `which-func'."
  (setq-local which-func-functions '(vhdl-ts-which-func-function))
  (setq-local which-func-format
              `("["
                (:propertize which-func-current
                 face (which-func :weight bold)
                 mouse-face mode-line-highlight)
                ":"
                (:propertize vhdl-ts-which-func-extra
                 face which-func
                 mouse-face mode-line-highlight)
                "]")))


;;; Navigation
(defconst vhdl-ts--defun-type-regexp
  (eval-when-compile
    (regexp-opt
     '("entity_declaration"
       "architecture_body"
       "package_declaration"
       "package_body"
       "process_statement"
       "procedure_declaration"
       "procedure_body"
       "function_body"))))

(defun vhdl-ts-find-function-procedure (&optional bwd)
  "Search for a VHDL function/procedure declaration or definition.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (vhdl-ts--node-at-point) "\\(function\\|procedure\\)_body" t bwd))

(defun vhdl-ts-find-function-procedure-fwd ()
  "Search forward for a VHDL function/procedure definition."
  (vhdl-ts-find-function-procedure))

(defun vhdl-ts-find-function-procedure-bwd ()
  "Search backward for a VHDL function/procedure definition."
  (vhdl-ts-find-function-procedure :bwd))

(defun vhdl-ts-find-block (&optional bwd)
  "Search for a VHDL block regexp, determined by `vhdl-ts-block-at-point-re'.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (vhdl-ts--node-at-point) vhdl-ts-block-at-point-re t bwd))

(defun vhdl-ts-find-block-fwd ()
  "Search forward for a VHDL block regexp."
  (vhdl-ts-find-block))

(defun vhdl-ts-find-block-bwd ()
  "Search backwards for a VHDL block regexp."
  (vhdl-ts-find-block :bwd))

(defun vhdl-ts-find-entity-instance (&optional bwd)
  "Search for a VHDL module/instance.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (vhdl-ts--node-at-point) vhdl-ts-instance-re t bwd))

(defun vhdl-ts-find-entity-instance-fwd ()
  "Search forwards for a VHDL module/instance."
  (interactive)
  (vhdl-ts-find-entity-instance))

(defun vhdl-ts-find-entity-instance-bwd ()
  "Search backwards for a VHDL module/instance."
  (interactive)
  (vhdl-ts-find-entity-instance :bwd))

(defun vhdl-ts-goto-next-error ()
  "Move point to next tree-sitter parsing error."
  (interactive)
  (treesit-search-forward-goto (vhdl-ts--node-at-point) "ERROR" t))

(defun vhdl-ts-goto-prev-error ()
  "Move point to previous tree-sitter parsing error."
  (interactive)
  (treesit-search-forward-goto (vhdl-ts--node-at-point) "ERROR" t :bwd))


;;; Completion
(defun vhdl-ts-completion-at-point ()
  "VHDL tree-sitter powered completion at point.

Complete with keywords and current buffer identifiers."
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds))
         candidates)
    (setq candidates (remove (thing-at-point 'symbol :no-props)
                             (append (mapcar (lambda (node-and-props)
                                               (plist-get (cdr node-and-props) :name))
                                             (vhdl-ts-nodes-props vhdl-ts-identifier-re))
                                     vhdl-ts-keywords
                                     vhdl-ts-types
                                     vhdl-ts-types-regexp
                                     vhdl-ts-attributes
                                     vhdl-ts-enum-values
                                     vhdl-ts-constants
                                     vhdl-ts-functions
                                     vhdl-ts-functions-regexp
                                     vhdl-ts-packages
                                     vhdl-ts-directives)))
    (list start end candidates . nil)))


;;; Major-mode
;;;; Setup
;;;###autoload
(defun vhdl-ts-install-grammar ()
  "Install VHDL tree-sitter grammar.

This command requires Git, a C compiler and (sometimes) a C++ compiler,
and the linker to be installed and on PATH."
  (interactive)
  (let ((url "https://github.com/alemuller/tree-sitter-vhdl"))
    (add-to-list 'treesit-language-source-alist `(vhdl ,url))
    (treesit-install-language-grammar 'vhdl)))

(defvar-keymap vhdl-ts-mode-map
  :doc "Keymap for VHDL language with tree-sitter"
  :parent vhdl-mode-map
  "TAB"     #'indent-for-tab-command
  "C-M-a"   #'beginning-of-defun
  "C-M-e"   #'end-of-defun
  "C-M-f"   #'vhdl-ts-forward-sexp
  "C-M-b"   #'vhdl-ts-backward-sexp
  "C-M-d"   #'vhdl-ts-find-entity-instance-fwd
  "C-M-u"   #'vhdl-ts-find-entity-instance-bwd
  "C-c e n" #'vhdl-ts-goto-next-error
  "C-c e p" #'vhdl-ts-goto-prev-error)

(defvar vhdl-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\"     table)
    (modify-syntax-entry ?+  "."      table)
    (modify-syntax-entry ?-  "."      table)
    (modify-syntax-entry ?=  "."      table)
    (modify-syntax-entry ?%  "."      table)
    (modify-syntax-entry ?<  "."      table)
    (modify-syntax-entry ?>  "."      table)
    (modify-syntax-entry ?&  "."      table)
    (modify-syntax-entry ?|  "."      table)
    (modify-syntax-entry ?`  "."      table)
    (modify-syntax-entry ?_  "_"      table)
    (modify-syntax-entry ?\' "."      table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    table)
  "Syntax table used in VHDL mode buffers.")

;;;###autoload
(define-derived-mode vhdl-ts-mode vhdl-mode "VHDL"
  "Major mode for editing VHDL files, using tree-sitter library."
  :syntax-table vhdl-mode-syntax-table
  ;; Treesit
  (when (treesit-ready-p 'vhdl)
    (treesit-parser-create 'vhdl)
    ;; Font-lock.
    (setq font-lock-defaults nil) ; Disable `vhdl-mode' font-lock/indent config
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword operator punctuation function builtin types)
                  (all)
                  (error)))
    (setq-local treesit-font-lock-settings vhdl-ts--treesit-settings)
    ;; Indent.
    (setq-local indent-line-function nil)
    (setq-local comment-indent-function nil)
    (setq-local treesit-simple-indent-rules vhdl-ts--indent-rules)
    ;; Navigation
    (setq-local treesit-defun-type-regexp vhdl-ts--defun-type-regexp)
    ;; Imenu.
    (setq-local imenu-create-index-function #'vhdl-ts-imenu-create-index)
    ;; Which-func
    (vhdl-ts-which-func)
    ;; Completion
    (add-hook 'completion-at-point-functions #'vhdl-ts-completion-at-point nil 'local)
    ;; Setup
    (treesit-major-mode-setup)))


;;; Provide
(provide 'vhdl-ts-mode)


;;; vhdl-ts-mode.el ends here
