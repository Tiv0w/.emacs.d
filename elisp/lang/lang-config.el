;;; elisp/lang/lang-config.el -*- lexical-binding: t; -*-

;;; For config files: YAML, JSON, TOML

(use-package json-mode
  :mode "\\.json$\\'"
  :config
  (setq json-reformat:indent-width 2
	js-indent-level 2))

(use-package yaml-mode
  :mode "\\.yml$\\'")

(use-package toml-mode
  :mode "\\.toml$\\'")

(provide 'lang-config)
