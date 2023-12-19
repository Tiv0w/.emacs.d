;;; elisp/lang/lang-svelte.el -*- lexical-binding: t; -*-


(use-package svelte-mode
  :defer t
  :mode "\\.svelte\\'")

(use-package tree-sitter
  :ensure t
  :hook (svelte-mode . tree-sitter-hl-mode)
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
	       '(svelte-mode . svelte)))

(provide 'lang-svelte)
