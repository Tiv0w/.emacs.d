;;; elisp/modules/t--lsp.el -*- lexical-binding: t; -*-
;;; Commentary:
;; LSP (and DAP) setup.
;; Every mode that wants to utilize LSP should hook `lsp-deferred'.

;;; Code:

(use-package lsp-mode
  :hook (lsp-mode . lsp-lens-mode)
  :config
  (setq
   ;; LSP performance tuning
   read-process-output-max (* 1024 1024) ;; 1mb
   gc-cons-threshold 104857600 ; 100mb
   lsp-prefer-flymake nil
   ;; Makes LSP shutdown the server when all buffers in the project are closed.
   lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-diagnostic-max-lines 2
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
	lsp-ui-peek-enable t
	lsp-ui-doc-max-height 8
	lsp-ui-doc-max-width 72 ; default 150
	lsp-ui-doc-delay 0.75	; default 0.2
	lsp-ui-doc-show-with-mouse nil
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(provide 't--lsp)
