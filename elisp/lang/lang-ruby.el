;;; elisp/lang/lang-ruby.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :hook (ruby-mode . lsp)
  :config
  (setq lsp-solargraph-use-bundler t
        lsp-sorbet-use-bundler t
        lsp-steep-use-bundler t
        lsp-typeprof-use-bundler t))

(use-package rbenv
  :config
  (add-to-list 'exec-path (expand-file-name "shims" rbenv-installation-dir)))

(use-package yard-mode
  :hook ruby-mode)

(use-package robe
  :hook (ruby-mode . robe-mode)
  :after yard-mode
  :config
  (push 'company-robe company-backends))

(use-package bundler
  :hook (ruby-mode . bundler-mode))

(use-package rinari
  :hook (ruby-mode . rinari-minor-mode))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))

(provide 'lang-ruby)
