;;; elisp/lang/lang-python.el -*- lexical-binding: t; -*-

;;; Python configuration

(use-package python-mode
  :ensure nil
  :mode-hydra
  (python-mode
   (:title "Python" :color blue :quit-key "q")
   ("Essential"
    (("a" run-python "start interpreter")
     ("s" python-shell-switch-to-shell "switch to repl"))
    "Eval"
    (("ee" python-shell-send-statement "statement")
     ("ed" python-shell-send-defun "defun")
     ("eb" python-shell-send-buffer "buffer")
     ("er" python-shell-send-region "region")))))

(use-package elpy
  :hook (python-mode . elpy-enable)
  :after python-mode
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-rpc-backend "jedi")
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  ;;flycheck-python-flake8-executable "/usr/local/bin/flake8"
  )

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8)


(use-package pyvenv
  :commands (pyvenv-activate pyvenv-workon)
  :config
   ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package pyvenv-auto
  :hook (python-mode . pyvenv-auto-mode)
  :config
  (setq pyvenv-auto-venv-dirnames '("venv" ".venv" "env")))


;; (set-pretty-symbols! 'python-mode
;;   ;; Functional
;;   :def "def"
;;   :lambda "lambda"
;;   ;; Types
;;   :null "None"
;;   :true "True"
;;   :false "False"
;;   :int "int"
;;   :str "str"
;;   :float "float"
;;   :bool "bool"
;;   :tuple "tuple"
;;   ;; Flow
;;   :not "not"
;;   :in "in"
;;   :not-in "not in"
;;   :and "and"
;;   :or "or"
;;   :for "for"
;;   :return "return"
;;   :yield "yield")

(provide 'lang-python)
;;; base-python.el ends here
