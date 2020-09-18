;;; elisp/lang/lang-php.el -*- lexical-binding: t; -*-

(use-package company-php
  :after company
  :config
  (add-to-list 'company-backends 'company-ac-php-backend))


(provide 'lang-php)
