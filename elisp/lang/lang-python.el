;;; elisp/lang/lang-python.el -*- lexical-binding: t; -*-

;;; Python configuration

(use-package elpy
  :mode ("\\.py$" . python-mode)
  :hook python-mode
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-rpc-backend "jedi")
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  ;;flycheck-python-flake8-executable "/usr/local/bin/flake8"
  (elpy-enable)
  :bind (:map elpy-mode-map
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark)))

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8)


(use-package pyvenv
  :commands pyvenv-activate)


(set-pretty-symbols! 'python-mode
  ;; Functional
  :def "def"
  :lambda "lambda"
  ;; Types
  :null "None"
  :true "True"
  :false "False"
  :int "int"
  :str "str"
  :float "float"
  :bool "bool"
  :tuple "tuple"
  ;; Flow
  :not "not"
  :in "in"
  :not-in "not in"
  :and "and"
  :or "or"
  :for "for"
  :return "return"
  :yield "yield")

(provide 'lang-python)
;;; base-python.el ends here
