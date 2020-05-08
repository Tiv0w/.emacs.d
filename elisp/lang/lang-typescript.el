;;; elisp/lang/lang-typescript.el -*- lexical-binding: t; -*-

;;; My setup for TypeScript support

(use-package typescript-mode
  :mode-hydra
  ((:title "Typescript-mode" :color blue :quit-key "q")
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
     ("t" tide-add-eslint-disable-next-line "eslint disable"))))
  :config
  (setq typescript-indent-level 2)
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
  (if (executable-find "eslint")
   (flycheck-add-mode 'javascript-eslint 'typescript-mode)
   (flycheck-select-checker 'javascript-eslint))
  (flycheck-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck add-node-modules-path)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (eldoc-mode +1)

  (defconst tide-eslint-disable-next-line-regexp
    "\\s *//\\s *eslint-disable-next-line\\s *:\\(.*\\)"
    "Regexp matching an eslint flag disabling rules on the next line.")

  (defun tide-add-eslint-disable-next-line ()
    "Add an eslint flag to disable rules generating errors at point."
    (interactive)
    (let ((error-ids (delq nil (tide-get-flycheck-errors-ids-at-point)))
          (start (point)))
      (when error-ids
        (save-excursion
          (if (and (eq 0 (forward-line -1))
                   (looking-at tide-eslint-disable-next-line-regexp))
              ;; We'll update the old flag.
              (let ((old-list (split-string (match-string 1))))
                (delete-region (point) (point-at-eol))
                (setq error-ids (append old-list error-ids)))
            ;; We'll create a new flag.
            (goto-char start)
            (beginning-of-line)
            (open-line 1))
          (insert "// eslint-disable-next-line:"
                  (string-join error-ids " "))
          (typescript-indent-line))))))

(use-package eslintd-fix
  :hook (typescript-mode . eslintd-fix-mode))

(provide 'lang-typescript)
