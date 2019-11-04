
(use-package clojure-mode
  :mode-hydra
  ((:title "Clojure" :color blue :quit-key "q")
   ("Essential"
    (("a" cider-jack-in "jack-in !")
     ("b" cider-load-buffer "load buffer")
     ("t" cider-test-run-test "run tests")
     ("e" cider-eval-last-sexp-to-repl "eval")
     ("d" cider-doc "doc")
     ("s" cider-switch-to-repl "switch to repl"))
    "Convert coll"
    (("(" clojure-convert-collection-to-list "coll -> (")
     ("{" clojure-convert-collection-to-map "coll -> {")
     ("[" clojure-convert-collection-to-vector "coll -> [")
     ("#" clojure-convert-collection-to-set "coll -> #")
     ("'" clojure-convert-collection-to-quoted-list "coll -> '("))
    "Repl"
    (("i" cider-repl-backward-input "previous input" :color red)
     ("k" cider-repl-forward-input "next input" :color red)
     ("c" cider-interrupt "cancel eval")))))

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
  :def     "defn"
  :null    "nil"
  :or      "or"
  :and     "and"
  :not     "not")


(provide 'lang-clojure)
