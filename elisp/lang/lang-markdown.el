;;; elisp/lang/lang-markdown.el -*- lexical-binding: t; -*-

;; markdown-mode setup
(use-package markdown-mode
  :defer t
  :mode "\\.md$\\'"
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
(use-package markdown-toc
  :after markdown-mode)

;; super simple live preview, with the flymd-flyit command
(use-package flymd
  :after markdown-mode
  :commands (flymd-flyit)
  :config
  (defun my-flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my-flymd-browser-function))

(use-package darkroom
  :after markdown-mode
  :commands (darkroom-mode darkroom-tentative-mode)
  :config
  (add-hook 'darkroom-mode-hook #'visual-line-mode))

(provide 'lang-markdown)
