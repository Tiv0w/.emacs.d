;;; elisp/lang/lang-config.el -*- lexical-binding: t; -*-

;;; For config files: YAML, JSON, TOML

(use-package json-mode
  :defer t
  :mode "\\.json$\\'"
  :hook (json-mode . rainbow-mode)
  :config
  (setq json-reformat:indent-width 2
        js-indent-level 2
        json-mode-indent-level 2))

(use-package yaml-mode
  :defer t
  :mode "\\.yml$\\'"
  :hook (yaml-mode . rainbow-mode))

(use-package toml-mode
  :defer t
  :mode "\\.toml$\\'"
  :hook (toml-mode . rainbow-mode))

(provide 'lang-config)
