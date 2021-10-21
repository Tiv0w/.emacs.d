;;; elisp/lang/lang-coq.el -*- lexical-binding: t; -*-


(use-package proof-general
  :mode-hydra
  (coq-mode
   (:title "Coq" :color red :quit-key "q")
   ("Essential"
    (("k" proof-assert-next-command-interactive "next command")
     ("i" proof-undo-last-successful-command "undo last")
     ("b" proof-process-buffer "process buffer" :color blue)
     ("a" proof-shell-start "start assistant" :color blue)))))


(provide 'lang-coq)
