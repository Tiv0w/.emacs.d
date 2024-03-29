;;; elisp/lang/lang-go.el -*- lexical-binding: t; -*-

(use-package go-mode
  :defer t
  :config
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook 'company-mode)
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook (lambda ()
			    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook (lambda ()
			    (local-set-key (kbd "C-c C-g") 'go-goto-imports)))
  (add-hook 'go-mode-hook (lambda ()
			    (set (make-local-variable 'company-backends) '(company-go))
			    (company-mode))))

(use-package company-go
  :after go-mode
  :config
  (setq tab-width 4)
  :bind (:map go-mode-map ; Godef jump key binding
	      ("M-." . godef-jump)))

(use-package flymake-go
  :after (go-mode flymake))

(use-package go-eldoc
  :after (go-mode eldoc)
  :hook (go-mode . go-eldoc-setup))

(defun setup-go-mode-compile ()
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(provide 'lang-go)
