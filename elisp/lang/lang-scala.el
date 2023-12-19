;;; elisp/lang/lang-scala.el -*- lexical-binding: t; -*-


;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :mode ("\\.sc\\'" . scala-mode)
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  ;; (substitute-key-definition
  ;;  'minibuffer-complete-word
  ;;  'self-insert-command
  ;;  minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Use tree-sitter for syntax highlighting
(use-package tree-sitter
  :ensure t
  :hook (scala-mode . tree-sitter-hl-mode))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :hook (scala-mode . flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  ;; You could also swap out lsp for lsp-deffered in order to defer loading
  :hook  (scala-mode . lsp-deferred)
         (lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-prefer-flymake nil)
  ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
  (setq lsp-keep-workspace-alive nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :after (scala-mode lsp-mode))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(provide 'lang-scala)
