;;; elisp/lang/lang-scala.el -*- lexical-binding: t; -*-


;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :mode ("\\.sc\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook #'lsp-deferred))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :after (scala-mode lsp-mode)
  :config
  (setq-local lsp-eldoc-exclude-line-regexps '("^Expression type:$")))

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

(provide 'lang-scala)
