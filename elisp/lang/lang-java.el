;;; elisp/lang/lang-java.el -*- lexical-binding: t; -*-


(major-mode-hydra-define java-mode
  (:title "Java" :color blue :quit-key "q")
  ("Navigation"
   (("m" meghanada-jump-declaration "jump to def")
    ("," meghanada-back-jump "jump back")
    ("." meghanada-jump-symbol "jump to impl"))
   "Imports"
   (("i" meghanada-import-at-point "import at point")
    ("o" meghanada-optimize-import "optimize import")
    ("u" meghanada-import-all "import all"))
   "Refactors"
   (("f" eglot-code-actions "eglot actions")
    ("r" eglot-rename "rename"))
   "Misc"
   (("b" meghanada-code-beautify "beautify code")
    ("l" meghanada-local-variable "local variable"))))

(use-package meghanada
  ;; :after java-mode
  :hook (java-mode . meghanada-mode)
  :config
  (setq meghanada-java-path "java"))

(use-package flycheck
  :hook (meghanada-mode . flycheck-mode))

(use-package groovy-mode)

(use-package kotlin-mode
  :mode "\\.kts?\\'")


(provide 'lang-java)
