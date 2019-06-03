;; vue
;; A setup with web-mode and mode-on-region.el for js2 on the script part

;; TODO: write a wrapper to mode-on-region to automatically
;; detect the script part and send it to mor-mode-on-region

(defun my-vue-mode-hook ()
  (use-package flycheck
    :config
    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers
		  (append flycheck-disabled-checkers
			  '(javascript-jshint))))

  (add-hook 'js2-mode-hook
            (defun my-js2-mode-setup ()
              (flycheck-mode t)
              (when (executable-find "eslint")
		(flycheck-select-checker 'javascript-eslint))))


  ;; (require mode-on-region)

  ;; (eval-after-load 'mode-on-region
  ;;   '(progn
  ;;      (setq mor-format-automatically-p nil)
  ;;      (setq mor-readonly-for-extra-protection-p t)
  ;;      (custom-set-faces
  ;; 	`(mor-readonly-face
  ;; 	  ((t (:background "black" :foreground "red" :strike-through t))))))))

  ;; (defun edit-region-in-js-mode (beg end)
  ;;   (interactive "@r")
  ;;   (let ((new-buffer (clone-indirect-buffer nil t)))
  ;;     (narrow-to-region beg end)
  ;;     (js2-mode))))


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
