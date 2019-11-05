;; typescript
;; A setup for TypeScript support

(use-package typescript-mode
  :mode-hydra
  ((:title "Typescript-mode" :color red :quit-key "q")
   ("Navigation"
    (("m" tide-jump-to-definition "jump to def")
     ("," tide-jump-back "jump back")
     ("." tide-jump-to-implementation "jump to impl"))
    "Editing"
    (("s" tide-rename-symbol "rename symbol" :color blue)
     ("d" tide-rename-file "rename file" :color blue)
     ("f" tide-fix "fix" :color blue)
     ("r" tide-refactor "refactor" :color blue)
     ("s" tide-organize-imports "select" :color blue)
     ("t" tide-add-tslint-disable-next-line "tslint disable" :color blue))))
  :config
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

(use-package add-node-modules-path
  :hook typescript-mode)

(use-package flycheck
  :hook (typescript-mode . flycheck-mode)
  :after (typescript-mode add-node-modules-path)
  :config
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  ;; we prefer tslint to eslint, hence the order
  (cond ;; ((executable-find "tslint")
	;;  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
	;;  (flycheck-select-checker 'typescript-tslint))
	((executable-find "eslint")
	 (flycheck-add-mode 'javascript-eslint 'typescript-mode)
	 (flycheck-select-checker 'javascript-eslint)))
  (flycheck-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck add-node-modules-path)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (eldoc-mode +1))

(use-package eslintd-fix
  :hook (typescript-mode . eslintd-fix-mode))

(provide 'lang-typescript)
