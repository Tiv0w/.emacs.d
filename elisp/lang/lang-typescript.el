;; typescript
;; A setup for TypeScript support

(use-package typescript-mode
  :config
  (use-package add-node-modules-path
    :hook typescript-mode)
  (use-package flycheck
    :hook (typescript-mode . flycheck-mode)
    :after add-node-modules-path
    :config
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    ;; we prefer tslint to eslint, hence the order
    (cond ((executable-find "tslint")
           (flycheck-add-mode 'typescript-tslint 'typescript-mode)
           (flycheck-select-checker 'typescript-tslint))
          ((executable-find "eslint")
           (flycheck-add-mode 'javascript-eslint 'typescript-mode)
           (flycheck-select-checker 'javascript-eslint)))
    (flycheck-mode))

  (set-pretty-symbols! 'typescript-mode
    ;; Functional
    :def "function"
    :lambda "() =>"
    :composition "compose"
    ;; Types
    :null "null"
    :true "true" :false "false"
    :int "number"
    :str "string"
    :bool "boolean"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return" :yield "import"))

(use-package eslintd-fix
  :hook (typescript-mode . eslintd-fix-mode))

(provide 'lang-typescript)
