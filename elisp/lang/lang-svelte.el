;;; elisp/lang/lang-svelte.el -*- lexical-binding: t; -*-


;; (use-package svelte-mode
;;   :defer t
;;   :mode "\\.svelte\\'")

(use-package web-mode
  :init
  (define-derived-mode svelte-web-mode web-mode "svelte-web"
    "Major mode for svelte based on web-mode."
    (setq-local web-mode-script-padding 1
                web-mode-style-padding 1
                web-mode-part-padding 0
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-enable-engine-detection t
                web-mode-enable-auto-closing t
                tree-sitter-hl-use-font-lock-keywords nil)))

(use-package svelte-web-mode
  :ensure nil
  :after web-mode
  :mode "\\.svelte\\'"
  :hook
  (svelte-web-mode . apheleia-mode)
  (svelte-web-mode . lsp)
  ;; :init
  ;; (push '(svelte-web-mode . prettier-svelte) apheleia-mode-alist)
  )

(use-package tree-sitter
  :ensure t
  :hook (svelte-web-mode . tree-sitter-hl-mode)
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(svelte-web-mode . svelte)))

(provide 'lang-svelte)
