;; ============================================================================
;;                           Color Theme
;; ============================================================================
;; Prot's themes have been reliably legible in nearly every situation.
(setq ef-themes-headings ; read the manual's entry of the doc string
      '((0 . (1.9))
        (1 . (1.8))
        (2 . (1.7))
        (3 . (1.6))
        (4 . (1.5))
        (5 . (1.4)) ; absence of weight means `bold'
        (6 . (1.3))
        (7 . (1.2))
        (t . (1.1))))

(setq ef-themes-to-toggle '(ef-light ef-trio-dark))

;; TODO: load based on time of day
;; (load-theme 'ef-trio-dark :no-confirm)
(load-theme 'ef-cherie :no-confirm)

;; LOVE these:
;; Light:
;; - ef-frost
;; - ef-light
;;
;; Dark:
;; - ef-cherie
;; - ef-trio-dark
;; - ef-winter

;; Nord doesn't look super great in org-mode.  Especially with source blocks
;; (require 'nord-theme)
;; (load-theme 'nord :no-confirm)

(set-face-attribute 'default nil :family "Iosevka")
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile")

(with-eval-after-load 'org-modern
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))
