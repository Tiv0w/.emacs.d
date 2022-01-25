;;; elisp/lang/lang-clojure.el -*- lexical-binding: t; -*-

(use-package clojure-mode
  :mode-hydra
  ((:title "Clojure" :color blue :quit-key "q")
   ("Essential"
    (("a" cider-jack-in "jack-in !")
     ("t" cider-test-run-test "run tests")
     ("w" cider-inspect "inspect")
     ("d" cider-doc "doc")
     ("v" cider-find-var "view source")
     ("s" cider-switch-to-repl "switch to repl"))
    "Eval"
    (("ee" cider-eval-last-sexp "last sexp")
     ("ed" cider-eval-defun-at-point "defun")
     ("eb" cider-load-buffer "buffer")
     ("en" cider-eval-ns-form "ns form"))
    "Convert coll"
    (("(" clojure-convert-collection-to-list "coll -> (")
     ("{" clojure-convert-collection-to-map "coll -> {")
     ("[" clojure-convert-collection-to-vector "coll -> [")
     ("#" clojure-convert-collection-to-set "coll -> #")
     ("'" clojure-convert-collection-to-quoted-list "coll -> '(")))))

(use-package cider
  :hook (clojure-mode-local-vars . cider-mode)
  :config
  (setq cider-repl-display-help-banner nil)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  :mode-hydra
  (cider-repl-mode
   (:color blue :quit-key "q")
   ("Cider REPL"
    (("i" cider-repl-backward-input "previous input" :color red)
     ("k" cider-repl-forward-input "next input" :color red)
     ("c" cider-interrupt "cancel eval")
     ("d" cider-repl-clear-buffer "clear buffer")))))

(use-package clojure-snippets
  :after (clojure-mode yasnippet))

(use-package clj-refactor)

(use-package kibit-helper)

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

(use-package flycheck
  :hook (clojure-mode . flycheck-mode)
  :config
  (use-package flycheck-clj-kondo))


(set-pretty-symbols! 'clojure-mode
  :lambda  "fn"
  :map     "map"
  :def     "defn"
  :true    "true"
  :false   "false"
  :null    "nil"
  :or      "or"
  :and     "and"
  :not     "not")


(provide 'lang-clojure)
