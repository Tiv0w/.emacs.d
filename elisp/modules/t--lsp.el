;;; elisp/modules/t--lsp.el -*- lexical-binding: t; -*-
;;; Commentary:
;; LSP (and DAP) setup.
;; Every mode that wants to utilize LSP should hook `lsp-deferred'.

;;; Code:

;; TODO: create keybindings

(use-package lsp-mode
  :hook (lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-modeline-code-actions-segments '(count icon name)
	lsp-signature-doc-lines 3
	;; Might need some per language server tweaking
	lsp-completion-show-detail nil
	lsp-completion-show-kind nil

	;; LSP performance tuning
	read-process-output-max (* 3 1024 1024) ;; 3mb
	gc-cons-threshold 104857600 ; 100mb
	lsp-prefer-flymake nil
	;; Makes LSP shutdown the server when all project buffers are closed.
	lsp-keep-workspace-alive nil)

  ;; HACK: some patch for LSP-mode, mainly for Metals
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents _server-id)
    "Extract a representative line from CONTENTS, to show in the echo area."
    (s-join
     "\n"
     (take lsp-eldoc-render-lines-count
           (seq-filter
            (lambda (line)
              (not (seq-some
                    (lambda (regexp)
                      (string-match-p regexp line))
                    lsp-eldoc-exclude-line-regexps)))
            (s-lines (s-trim (lsp--render-element contents)))))))

  (defcustom lsp-eldoc-render-lines-count 1
    "Display at most x lines of info returned by document/onHover.
If this is set to 1, `eldoc' will show only the symbol information.
Useful for LSPs that format differently their output."
    :type 'number
    :group 'lsp-mode)

  (defcustom lsp-eldoc-exclude-line-regexps '()
    "Customize the lines that should be excluded from the text returned
by document/onHover.
Useful for LSPs that format differently their output."
    :type '(repeat regexp)
    :group 'lsp-mode)

  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-diagnostic-max-lines 2
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-peek-enable t
        lsp-ui-doc-max-height 20 ; default 13
        lsp-ui-doc-max-width 72 ; default 150
        lsp-ui-doc-delay 0.75   ; default 0.2
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
