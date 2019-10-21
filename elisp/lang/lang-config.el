;; config
;; For config files: YAML, JSON

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist
	       '("\\.json$" . json-mode)))


(provide 'lang-config)
