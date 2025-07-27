;;; lang-config.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; For config files: YAML, JSON, TOML

;;; Code:

(use-package json-mode
  :defer t
  ;; :mode "\\.json$\\'"
  :hook ((json-mode json-ts-mode) . rainbow-mode)
  :config
  (setq json-reformat:indent-width 2
        js-indent-level 2
        json-mode-indent-level 2))

(use-package yaml-mode
  :defer t
  ;; :mode "\\.yml$\\'"
  :hook ((yaml-mode yaml-ts-mode) . rainbow-mode))

(use-package toml-mode
  :defer t
  ;; :mode "\\.toml$\\'"
  :hook
  ((toml-mode toml-ts-mode) . rainbow-mode)
  ;; ((toml-mode toml-ts-mode) . apheleia-mode)
  :config
  ;; (t--set-formatter 'taplo '("taplo" "format" "--option" "column_width=200" "--option" "align_entries=true" "-") :modes '(toml-mode toml-ts-mode))
  )


(provide 'lang-config)
;;; lang-config.el ends here
