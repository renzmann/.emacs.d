(when (renz/windowsp)
    ;; Set a font that supports emoji
    (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji") nil 'prepend))
