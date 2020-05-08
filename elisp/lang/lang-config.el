;;; elisp/lang/lang-config.el -*- lexical-binding: t; -*-

;;; For config files: YAML, JSON, TOML

(use-package json-mode
  :mode "\\.json$\\'")

(use-package yaml-mode
  :mode "\\.yml$\\'")

(use-package toml-mode
  :mode "\\.toml$\\'")

(provide 'lang-config)
