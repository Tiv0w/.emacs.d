;;; elisp/lang/lang-scheme.el -*- lexical-binding: t; -*-


(use-package scheme-mode
  :ensure nil
  :mode "\\.egg\\'"
  :mode-hydra
  (scheme-mode
   (:title "Scheme" :color blue :quit-key "q")
   ("Essential"
    (("a" run-geiser "run-geiser")
     ("c" geiser-compile-current-buffer "compile buffer")
     ("s" geiser-mode-switch-to-repl "switch to repl"))
    "Eval"
    (("ee" geiser-eval-last-sexp "last sexp")
     ("ed" geiser-eval-definition "defun")
     ("eb" geiser-eval-buffer "buffer"))
    "Misc"
    (("(" geiser-squarify "() <-> []")))))

(use-package geiser
  :hook (scheme-mode . geiser-mode))

(use-package geiser-chicken
  :after geiser)

(use-package geiser-gambit
  :after geiser)


(provide 'lang-scheme)
