;;; elisp/modules/t--org-babel.el -*- lexical-binding: t; -*-
;;; Commentary:
; Org-babel setup.

;;; Code:

(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

;; Emacs builtins
(require 'ob-js)
(require 'ob-clojure)

(setq org-src-lang-modes
      (append '(("dot" . graphviz-dot))
              (delete '("dot" . fundamental) org-src-lang-modes)))

;; External packages
(use-package ob-typescript)
(use-package ob-restclient)

(provide 't--org-babel)
