;;; elisp/lang/lang-vue.el -*- lexical-binding: t; -*-

;; vue
;; A setup with web-mode and mode-on-region.el for js2 on the script part

;; TODO: write a wrapper to mode-on-region to automatically
;; detect the script part and send it to mor-mode-on-region

(defun my-vue-mode-hook ()
  (use-package flycheck
    :hook ((web-mode js2-mode) . flycheck-mode)
    :after add-node-modules-path
    :config
    ;; disable jshint since we prefer eslint checking
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    (when (executable-find "eslint")
      (flycheck-select-checker 'javascript-eslint))
    (flycheck-mode)))

(use-package flycheck
  :hook (web-mode . flycheck-mode)
  :after (web-mode add-node-modules-path)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (when (executable-find "eslint")
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-select-checker 'javascript-eslint))
  (flycheck-mode))


(defun my/use-eslint-from-node-modules ()
  "Gets eslint exe from local path."
  (let (eslint)
    (setq eslint (projectile-expand-root "node_modules/eslint/bin/eslint.js"))
    (setq flycheck-eslintrc (projectile-expand-root ".eslintrc.js"))
    (setq-default flycheck-javascript-eslint-executable eslint)))

(use-package add-node-modules-path
  :hook web-mode)

(use-package web-mode
  :mode "\\.vue\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;;  (setq web-mode-enable-auto-indentation t)
  ;;  (setq web-mode-indent-style 2)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-closing t))

(add-hook 'web-mode-hook #'my-vue-mode-hook)
(add-hook 'web-mode-hook #'my/use-eslint-from-node-modules)

(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))

(require 'lang-vue-helper)

(provide 'lang-vue)
