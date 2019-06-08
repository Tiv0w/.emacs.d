
(use-package clojure-mode)

(use-package cider
  :hook (clojure-mode-local-vars . cider-mode)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode))

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))


(set-pretty-symbols! 'clojure-mode
    :lambda  "fn"
    :map     "map"
    :def "defn"
    :null "nil"
    :or "or"
    :and "and"
    :not "not")


(provide 'lang-clojure)
