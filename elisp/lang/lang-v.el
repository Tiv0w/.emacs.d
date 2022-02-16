;;; elisp/lang/lang-v.el -*- lexical-binding: t; -*-


(use-package v-mode
  :load-path (lambda () (concat user-emacs-directory "elisp/extlisp/v-mode/"))
  :mode-hydra
  (v-mode
   (:title "V" :color blue :quit-key "q")
   ("Navigation"
    (("," eglot-find-declaration "jump to def")
     ("." eglot-find-implementation "jump to impl"))
    "Editing"
    (("s" eglot-rename "rename symbol")
     ("f" eglot-code-actions "code actions"))
    "Misc"
    (("b" eglot-format-buffer "format buffer")))))

(use-package yafolding
  :hook (v-mode . yafolding-mode))

(set-pretty-symbols! 'v-mode
  ;; Functional
  :def "fn"
  ;; Types
  ;; :null "NULL"
  :true "true"
  :false "false"
  :int "int"
  :float "float"
  :str "string"
  :bool "bool"
  ;; Flow
  :not "!"
  :and "&&"
  :or "||"
  :for "for"
  :return "return"
  :yield "import")


(provide 'lang-v)
