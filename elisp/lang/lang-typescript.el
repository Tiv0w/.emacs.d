;;; lang-typescript.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; My setup for TypeScript support

;;; Code:

(use-package typescript-mode
  :init
  ;; (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :hook
  ((typescript-mode ;; typescript-tsx-mode
                    typescript-ts-mode tsx-ts-mode) . lsp-deferred)
  ((typescript-mode ;; typescript-tsx-mode
                    typescript-ts-mode tsx-ts-mode) . t--env/setup-fnm-env)
  ((typescript-mode ;; typescript-tsx-mode
                    typescript-ts-mode tsx-ts-mode) . apheleia-mode)
  ((typescript-mode ;; typescript-tsx-mode
                    ) . tree-sitter-hl-mode)
  :config
  (setq typescript-indent-level 4)
  ;; (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
  )

(use-package add-node-modules-path
  :hook ((typescript-mode ;; typescript-tsx-mode
                          ) . add-node-modules-path))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter
;;   :config
;;   (tree-sitter-require 'tsx)
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(provide 'lang-typescript)
;;; lang-typescript.el ends here
