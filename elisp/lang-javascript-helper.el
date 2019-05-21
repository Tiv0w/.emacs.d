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

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))


;; careful it does not take the this.
(defun log-this ()
  "Logs the thing at point."
  (interactive "")
  (save-excursion
    (save-restriction
      (let ((x (thing-at-point 'symbol)))
        (end-of-line)
        (newline)
        (insert (format "console.log('%s: ', %s)" x x))))))


(provide 'lang-javascript-helper)
