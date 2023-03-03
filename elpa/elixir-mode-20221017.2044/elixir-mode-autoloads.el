;;; elixir-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from elixir-format.el

(autoload 'elixir-format "elixir-format" "\


(fn &optional CALLED-INTERACTIVELY-P)" t)
(register-definition-prefixes "elixir-format" '("elixir-format-"))


;;; Generated autoloads from elixir-mode.el

(autoload 'elixir-mode-open-github "elixir-mode" "\
Elixir mode open GitHub page." t)
(autoload 'elixir-mode-open-elixir-home "elixir-mode" "\
Elixir mode go to language home." t)
(autoload 'elixir-mode-open-docs-master "elixir-mode" "\
Elixir mode go to master documentation." t)
(autoload 'elixir-mode-open-docs-stable "elixir-mode" "\
Elixir mode go to stable documentation." t)
(autoload 'elixir-mode-version "elixir-mode" "\
Get the Elixir-Mode version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

(fn &optional SHOW-VERSION)" t)
(autoload 'elixir-mode "elixir-mode" "\
Major mode for editing Elixir code.

\\{elixir-mode-map}

(fn)" t)
(add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode))
(register-definition-prefixes "elixir-mode" '("elixir-"))


;;; Generated autoloads from elixir-smie.el

(register-definition-prefixes "elixir-smie" '("elixir-" "verbose-elixir-smie-rules"))

;;; End of scraped data

(provide 'elixir-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; elixir-mode-autoloads.el ends here
