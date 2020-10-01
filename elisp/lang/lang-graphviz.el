;;; elisp/lang/lang-graphviz.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Graphviz setup.

;;; Code:


(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  :ensure graphviz-dot-mode)

(use-package gvpr-mode)

(provide 'lang-graphviz)
