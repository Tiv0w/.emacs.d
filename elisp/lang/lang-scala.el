;;; elisp/lang/lang-scala.el -*- lexical-binding: t; -*-


;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :mode ("\\.sc\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  :hook
  (scala-mode . apheleia-mode)
  (scala-mode . lsp-deferred)
  :config
  (setq-local company-idle-delay 0.7
	      company-minimum-prefix-length 2))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :after (scala-mode lsp-mode)
  :custom
  (lsp-metals-server-args '("-J-Dmetals.excluded-packages=scala.runtime.stdLibPatches,"))
  :init
  (lsp-interface
   (DidChangeConfigurationCapabilities nil (:dynamicRegistration)))
  :config
  (setq lsp-eldoc-exclude-line-regexps '("^Expression type:.*$"))
  ;; (setq lsp-log-io t)
  ;; (lsp-register-custom-settings
  ;;  '(("metals.excluded-packages" ("org.apache.pekko.actor.typed.javadsl"
  ;; 				  "scala.runtime.stdLibPatches.language"))))
  )

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
