;;; elisp/lang/lang-svelte.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Svelte files support through treesit and lsp-mode

;;; Code:

(use-package svelte-ts-mode
  :vc (:url "https://github.com/leafOfTree/svelte-ts-mode")
  :hook
  (svelte-ts-mode . apheleia-mode)
  (svelte-ts-mode . lsp-deferred))

(provide 'lang-svelte)
;;; lang-svelte.el ends here
