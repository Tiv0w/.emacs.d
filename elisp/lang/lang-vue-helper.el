;;; elisp/lang/lang-vue-helper.el -*- lexical-binding: t; -*-

;;; runs eslint --fix on the current file after save
;;; alpha quality -- use at your own risk

(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(add-hook 'web-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)) nil t)

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

(provide 'lang-vue-helper)
