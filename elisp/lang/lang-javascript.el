;;; elisp/lang/lang-javascript.el -*- lexical-binding: t; -*-

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :bind (:map js2-mode-map
              (("C-x C-e" . js-send-last-sexp)
               ("C-M-x" . js-send-last-sexp-and-go)
               ("C-c C-b" . js-send-buffer-and-go)
               ("C-c C-l" . js-load-file-and-go)))
  :mode
  ("\\.js$" . js2-mode)
  ;; ("\\.json$" . js2-jsx-mode)
  :config
  (custom-set-variables '(js2-strict-inconsistent-return-warning nil))
  (custom-set-variables '(js2-strict-missing-semi-warning nil))

  (setq js-indent-level 2)
  (setq js2-indent-level 2)
  (setq js2-basic-offset 2)
  :mode-hydra
  ((:title "JavaScript-mode" :color blue :quit-key "q")
   ("Navigation"
    (("m" tide-jump-to-definition "jump to def")
     ("," tide-jump-back "jump back")
     ("." tide-jump-to-implementation "jump to impl"))
    "Editing"
    (("s" tide-rename-symbol "rename symbol")
     ("a" tide-rename-file "rename file")
     ("f" tide-fix "fix")
     ("r" tide-refactor "refactor"))
    "Misc"
    (("o" tide-organize-imports "organize imports")
     ("d" tide-jsdoc-template "jsdoc")
     ("t" tide-add-eslint-disable-next-line "eslint disable")))))

;; Run a JavaScript interpreter in an inferior process window
;; https://github.com/redguardtoo/js-comint
(use-package js-comint
  :after js2-mode
  :config
  (setq inferior-js-program-command "node"))

;; js2-refactor :- refactoring options for emacs
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :after js2-mode
  ;; :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c j r")
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

(use-package add-node-modules-path
  :hook js2-mode)

(use-package flycheck
  :hook (js2-mode . flycheck-mode)
  :after (js2-mode add-node-modules-path)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (if (executable-find "eslint")
      (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-select-checker 'javascript-eslint))
  (flycheck-mode))

(use-package tide
  :ensure t
  :after (js2-mode company flycheck add-node-modules-path)
  :hook ((js2-mode . tide-setup)
	 (js2-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save))
  :config
  (eldoc-mode +1)
  (setq
   tide-native-json-parsing t
   tide-completion-detailed t
   tide-always-show-documentation t
   ;; By default, tide ignores payloads larger than 100kb. This is too small for
   ;; larger projects that produce long completion lists, so we up it to 512kb.
   tide-server-max-response-length 524288 )

  ;; ;; code completion
  ;; (after! company
  ;;   ;; tide affects the global `company-backends', undo this so doom can handle
  ;;   ;; it buffer-locally
  ;;   (setq-default company-backends (delq 'company-tide (default-value 'company-backends))))
  ;; (set-company-backend! 'tide-mode 'company-tide)
  ;; ;; navigation
  ;; (set-lookup-handlers! 'tide-mode :async t
  ;;   :xref-backend #'xref-tide-xref-backend
  ;;   :documentation #'tide-documentation-at-point)
  ;; (set-popup-rule! "^\\*tide-documentation" :quit t)
  ;; ;; resolve to `doom-project-root' if `tide-project-root' fails
  ;; (advice-add #'tide-project-root :override #'+javascript-tide-project-root-a)
  ;; ;; cleanup tsserver when no tide buffers are left
  ;; (add-hook! 'tide-mode-hook
  ;;   (add-hook 'kill-buffer-hook #'+javascript-cleanup-tide-processes-h nil t))

  ;; Eldoc is activated too soon and disables itself, thinking there is no eldoc
  ;; support in the current buffer, so we must re-enable it later once eldoc
  ;; support exists. It is set *after* tide-mode is enabled, so enabling it on
  ;; `tide-mode-hook' is too early, so...
  ;; (advice-add #'tide-setup :after #'eldoc-mode
  )


(set-pretty-symbols! '(js2-mode rjsx-mode web-mode js-mode)
  ;; Functional
  :def "function"
  :lambda "() =>"
  :composition "compose"
  ;; Types
  :null "null"
  :true "true"
  :false "false"
  ;; Flow
  :not "!"
  :and "&&"
  :or "||"
  :for "for"
  :return "return"
  ;; Other
  :yield "import")

(use-package eslintd-fix
  :hook (js2-mode . eslintd-fix-mode))

;; (require 'lang-javascript-helper)

(provide 'lang-javascript)
