;; ============================================================================
;;                           Color Theme
;; ============================================================================
;; Prot's themes have been reliably legible in nearly every situation.
;; (setq ef-themes-headings ; read the manual's entry of the doc string
;;       '((0 . (1.9))
;;         (1 . (1.8))
;;         (2 . (1.7))
;;         (3 . (1.6))
;;         (4 . (1.5))
;;         (5 . (1.4)) ; absence of weight means `bold'
;;         (6 . (1.3))
;;         (7 . (1.2))
;;         (t . (1.1))))

(setq ef-themes-to-toggle '(ef-day ef-night))

(if (display-graphic-p)
    ;; (load-theme 'ef-night :no-confirm)
    (load-theme 'doom-one :no-confirm)
  (load-theme 'ef-night :no-confirm))
