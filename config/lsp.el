;; ============================================================================
;; 			 LSP (Eglot lsp-mode)
;; ============================================================================
;; Mostly obsolete, since I use eglot now, which needs basically no
;; configuration.

;; (require 'lsp-mode)

;; (setq lsp-keymap-prefix "s-p")
;; (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
;;                      :major-modes '(python-mode)
;;                      :remote? t
;;                      :server-id 'pylsp-remote))

;; (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-tramp-connection "pyright-langserver --stdio")
;;                      :major-modes '(python-mode)
;;                      :remote? t
;;                      :server-id 'pyright-remote))

;; (defun lsp-tramp-connection (local-command &optional generate-error-file-fn)
;;     "Create LSP stdio connection named name.
;; LOCAL-COMMAND is either list of strings, string or function which
;; returns the command to execute."
;;     (defvar tramp-connection-properties)
;;     ;; Force a direct asynchronous process.
;;     (when (file-remote-p default-directory)
;;       (add-to-list 'tramp-connection-properties
;;                    (list (regexp-quote (file-remote-p default-directory))
;;                          "direct-async-process" t)))
;;     (list :connect (lambda (filter sentinel name environment-fn)
;;                      (let* ((final-command (lsp-resolve-final-function
;;                                             local-command))
;;                             (_stderr (or (when generate-error-file-fn
;;                                            (funcall generate-error-file-fn name))
;;                                          (format "/tmp/%s-%s-stderr" name
;;                                                  (cl-incf lsp--stderr-index))))
;;                             (process-name (generate-new-buffer-name name))
;;                             (process-environment
;;                              (lsp--compute-process-environment environment-fn))
;;                             (proc (make-process
;;                                    :name process-name
;;                                    :buffer (format "*%s*" process-name)
;;                                    :command final-command
;;                                    :connection-type 'pipe
;;                                    :coding 'no-conversion
;;                                    :noquery t
;;                                    :filter filter
;;                                    :sentinel sentinel
;;                                    :file-handler t)))
;;                        (cons proc proc)))
;;           :test? (lambda () (-> local-command lsp-resolve-final-function
;;                            lsp-server-present?))))

;; (setq lsp-completion-provider :none)

;; (defun renz/corfu-lsp-setup ()
;;   (setq-local completion-styles '(flex basic partial-completion emacs22)
;;               completion-category-defaults nil))

;; (add-hook 'lsp-mode-hook #'renz/corfu-lsp-setup)
