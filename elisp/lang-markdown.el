;;; lang-markdown

;; markdown-mode setup
(use-package markdown-mode
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("bash")
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil ; trigger with `markdown-toggle-url-hiding'
        markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-gfm-uppercase-checkbox t))

;; to generate markdown table of contents
(use-package markdown-toc)

;; super simple live preview, with the flymd-flyit command
(use-package flymd
  :config
  (defun my-flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my-flymd-browser-function))

(use-package flycheck
  :hook (markdown-mode . flycheck-mode))


(use-package flycheck-grammalecte
  :init
  (setq flycheck-grammalecte-enabled-modes
      '(org-mode text-mode mail-mode latex-mode markdown-mode)))

(use-package darkroom
  :hook (markdown-mode . darkroom-tentative-mode))

(provide 'lang-markdown)
