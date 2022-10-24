;; ============================================================================
;; 			      TreeSitter
;; ============================================================================
(unless (string= (getenv "HOSTNAME") "xhadrevrm7p.aetna.com")
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
