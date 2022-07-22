;;; elisp/lang/lang-vue.el -*- lexical-binding: t; -*-

(use-package web-mode
  :mode "\\.vue\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        ;; web-mode-enable-auto-indentation t
        ;; web-mode-indent-style 2
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-closing t)
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal) "//")
  (add-hook 'web-mode-hook #'vue-lsp-hook))

(use-package add-node-modules-path
  :hook web-mode)

(use-package lsp-mode)

(defun vue-lsp-hook ()
  (when (equal web-mode-engine "vue")
    (lsp)))

(defun log-at-point ()
  "console.logs the thing at point.
Does not take the `this' in JS, use a region for that."
  (interactive "")
  (save-excursion
    (let ((x (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (thing-at-point 'symbol))))
      (end-of-line)
      (reindent-then-newline-and-indent)
      (insert (format "console.log('%s: ', %s);" x x)))))


(provide 'lang-vue)
