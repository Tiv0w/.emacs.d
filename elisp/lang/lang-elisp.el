;;; elisp/lang/lang-elisp.el -*- lexical-binding: t; -*-

;;; Code:

(use-package emacs-lisp-mode
  :ensure nil
  :mode-hydra
  ((:title "Elisp" :color blue :quit-key "q")
   ("Eval"
    (("ee" eval-last-sexp "last sexp")
     ("ed" eval-defun "defun")
     ("eb" eval-buffer "buffer")
     ("er" eval-and-replace "eval and replace")))))

(provide 'lang-elisp)
;;; lang-elisp.el ends here
