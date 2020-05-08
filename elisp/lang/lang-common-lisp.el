;;; elisp/lang/lang-common-lisp.el -*- lexical-binding: t; -*-

(setq inferior-lisp-program "sbcl")

(use-package sly
  :hook lisp-mode)

(set-pretty-symbols! 'lisp-mode
  :lambda  "lambda"
  :map     "map"
  :def     "defun"
  :true    "t"
  :false   "nil"
  :or      "or"
  :and     "and"
  :not     "not"
  :return  "return"
  :some    "some")


(provide 'lang-common-lisp)
