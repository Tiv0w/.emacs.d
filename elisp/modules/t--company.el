;;; elisp/modules/t--company.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages setup up completion with company.


(use-package company
  :hook ((prog-mode comint-mode) . company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-require-match 'never))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-images))


(provide 't--company)
