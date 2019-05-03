;; vue

(defun my-vue-mode ()
  (use-package web-mode
    :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    ;;  (setq web-mode-enable-auto-indentation t)
    ;;  (setq web-mode-indent-style 2)
    (setq web-mode-enable-auto-indentation nil))

  (use-package flycheck
    :config
    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers
		  (append flycheck-disabled-checkers
			  '(javascript-jshint))))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (defun edit-region-in-js-mode (beg end)
    (interactive "@r")
    (let ((new-buffer (clone-indirect-buffer nil t)))
      (narrow-to-region beg end)
      (js2-mode)))

  )

(add-to-list 'auto-mode-alist '("\\.vue$" . my-vue-mode))

(provide 'lang-vue2)
