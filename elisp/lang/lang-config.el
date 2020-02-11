;; config
;; For config files: YAML, JSON

(use-package json-mode
  :mode "\\.json$\\'")

(use-package yaml-mode
  :mode "\\.yml$\\'")

(use-package toml-mode
  :mode "\\.toml$\\'")

(provide 'lang-config)
