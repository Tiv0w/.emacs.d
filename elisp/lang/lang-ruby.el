;;; elisp/lang/lang-ruby.el -*- lexical-binding: t; -*-

;;; Ruby configuration
;; Uses LSP with the solargraph server
;; If using bundler, add solargraph as a dev dependency
;; and run `yard gems' in the root of the repo.
;;
;; For robe, add pry, pry-doc and webrick as dev dependencies.

(use-package ruby-mode
  :ensure nil ; an Emacs built-in
  :mode-hydra
  ((:title "Ruby" :color blue :quit-key "q")
   ("Essential"
    (("a" robe-start "start robe")
     ("m" robe-jump "jump to def")
     ("d" robe-doc "doc"))
    "Eval"
    (("ee" ruby-send-last-stmt "last statement")
     ("ed" ruby-send-definition "def")
     ("eb" ruby-send-buffer "buffer"))
    "Rubocop"
    (("la" rubocop-autocorrect-current-file "autocorrect")
     ("lc" rubocop-check-current-file "check")
     ("lf" rubocop-format-current-file "format"))
    "Misc"
    (("{" ruby-toggle-block "block -> {")))))

(use-package lsp-mode
  :hook (ruby-mode . lsp)
  :config
  (setq lsp-solargraph-use-bundler t
        lsp-sorbet-use-bundler t
        lsp-steep-use-bundler t
        lsp-typeprof-use-bundler t))

(use-package rbenv
  :config
  (add-to-list 'exec-path (expand-file-name "shims/" rbenv-installation-dir)))

(use-package yard-mode
  :hook ruby-mode)

(use-package robe
  :hook (ruby-mode . robe-mode)
  :after yard-mode
  :config
  (push 'company-robe company-backends))

(use-package bundler
  :after ruby-mode)

(use-package rinari
  :hook (ruby-mode . rinari-minor-mode))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))

(provide 'lang-ruby)
