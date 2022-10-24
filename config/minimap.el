;; ============================================================================
;;                            Minimap Display
;; ============================================================================
(when (display-graphic-p)
  (minimap-mode t)
  (setq minimap-minimum-width 20)
  (setq minimap-width-fraction 0.1)
  (setq minimap-window-location 'right))
