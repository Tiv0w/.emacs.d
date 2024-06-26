;;; elisp/modules/t--tree-sitter.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This configures tree-sitter correctly.

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package fringe-helper
  :defer t)

(use-package ts-fold-indicators
  :vc (:fetcher github :repo "emacs-tree-sitter/ts-fold")
  :custom
  (ts-fold-replacement "   [...]   ")
  :custom-face
  (ts-fold-replacement-face ((t  (:inherit font-lock-comment-face
                                  :foreground unspecified
                                  :box nil
                                  :weight bold
                                  :slant italic))))
  :config
  (global-ts-fold-indicators-mode))

(provide 't--tree-sitter)
