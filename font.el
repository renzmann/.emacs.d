(set-face-attribute 'default nil :font "Hack Nerd Font Mono-11")

(when (renz/windowsp)
    ;; Set a font that supports emoji
    (set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji") nil 'prepend)
    (set-face-attribute 'default nil :font "Hack NF-12"))

(when (eq system-type 'darwin)
  ;; Uncomment this if we can't install Hack Nerd font
  ;; (set-face-attribute 'default nil :font "Menlo-14")
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-13"))
