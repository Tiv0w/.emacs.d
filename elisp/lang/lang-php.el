;;; elisp/lang/lang-php.el -*- lexical-binding: t; -*-

(use-package php-mode
  :mode "\\.php\\'")

(use-package company-php
  :after company)

(use-package php-extras
  :load-path "./elisp/extlisp/php-extras/"
  :after php
  :config
  (require 'php-extras-eldoc-functions php-extras-eldoc-functions-file t))


(use-package web-mode
  :mode "\\.phtml\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-script-padding 0
        web-mode-style-padding 0
        web-mode-enable-current-element-highlight t)

  (use-package web-mode-edit-element
    :after web-mode
    :hook (web-mode . web-mode-edit-element-minor-mode))

  (use-package emmet-mode
    :after web-mode
    :hook web-mode
    :config
    (setq emmet-move-cursor-between-quotes t))

  (use-package company-web
    :after company))


(defun t--php-company ()
  (set (make-local-variable 'company-backends)
       '((;; company-ac-php-backend
          php-extras-company
          company-web-html
          company-dabbrev-code
          ;; company-capf
          company-files))))
(add-hook 'php-mode-hook #'t--php-company)


(provide 'lang-php)
