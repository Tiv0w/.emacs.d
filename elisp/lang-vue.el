;; vue
;; A setup with web-mode and mode-on-region.el for js2 on the script part

;; TODO: write a wrapper to mode-on-region to automatically
;; detect the script part and send it to mor-mode-on-region

(defun my-vue-mode-hook ()
  (use-package flycheck
    :hook ((web-mode js2-mode) . flycheck-mode)
    :config
    ;; disable jshint since we prefer eslint checking
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    (when (executable-find "eslint")
      (flycheck-select-checker 'javascript-eslint))
    (flycheck-mode)))


(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;;  (setq web-mode-enable-auto-indentation t)
  ;;  (setq web-mode-indent-style 2)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-closing t))

(add-hook 'web-mode-hook 'my-vue-mode-hook)

(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))

(require 'lang-vue-helper)

(provide 'lang-vue)
