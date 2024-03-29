;;; elisp/lang/lang-java.el -*- lexical-binding: t; -*-


(major-mode-hydra-define java-mode
  (:title "Java" :color blue :quit-key "q")
  ("Navigation"
   (("m" lsp-find-definition "jump to def")
    ("," lsp-find-references "jump back")
    ("." lsp-find-implementation "jump to impl"))
   "Imports"
   (("i" lsp-java-add-import "add import")
    ("o" lsp-java-organize-imports "organize imports")
    ("u" meghanada-import-all "import all"))
   "Refactors"
   (("f" lsp-execute-code-action "lsp actions")
    ("r" lsp-rename "rename"))
   "Misc"
   (("b" meghanada-code-beautify "beautify code")
    ("l" lsp-java-extract-to-local-variable "local variable"))))

(use-package meghanada
  ;; :after java-mode
  :disabled
  :hook (java-mode . meghanada-mode)
  :config
  (setq meghanada-java-path "java"))


(use-package lsp-java
  :after (lsp-mode java-mode)
  :hook (java-mode . lsp-deferred)
  :config
  (setq lsp-java-format-enabled nil
        lsp-java-format-comments-enabled nil))

(use-package flycheck
  :hook (meghanada-mode . flycheck-mode))

(use-package groovy-mode
  :defer t)

(use-package kotlin-mode
  :defer t
  :mode "\\.kts?\\'")


(provide 'lang-java)
