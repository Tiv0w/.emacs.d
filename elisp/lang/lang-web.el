;;; elisp/lang/lang-web.el -*- lexical-binding: t; -*-

(use-package web-mode
  :defer t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.html.j2\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.ejs\\'" . web-mode)
   ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-script-padding 0
        web-mode-style-padding 0
        web-mode-enable-engine-detection t
        web-mode-enable-auto-closing t)

  (add-hook 'web-mode-hook 'jsx-flycheck)

  ;; highlight enclosing tags of the element under cursor
  (setq web-mode-enable-current-element-highlight t)

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (defun jsx-flycheck ()
    (when (equal web-mode-content-type "jsx")
      ;; enable flycheck
      (flycheck-select-checker 'jsxhint-checker)
      (flycheck-mode)))

  ;; editing enhancements for web-mode
  ;; https://github.com/jtkDvlp/web-mode-edit-element
  (use-package web-mode-edit-element
    :hook (web-mode . web-mode-edit-element-minor-mode))

  ;; snippets for HTML
  ;; https://github.com/smihica/emmet-mode
  (use-package emmet-mode
    :hook web-mode
    :diminish (emmet-mode . " e")
    :config
    (setq emmet-move-cursor-between-quotes t))

  (defun my-web-mode-hook ()
    "Hook for `web-mode' config for company-backends."
    (require 'company-bulma)
    (set (make-local-variable 'company-backends)
          '((company-bulma-backend company-css company-web-html company-files))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running company-tern."
    (message "advice")
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language
               (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx"))
              (unless tern-mode (tern-mode))
            (if tern-mode (tern-mode -1))))))
  (add-hook 'web-mode-hook 'company-mode)

  ;; to get completion for HTML stuff
  ;; https://github.com/osv/company-web
  (use-package company-web
    :after company)

  (add-hook 'web-mode-hook 'company-mode)


  :mode-hydra
  ((:title "Web-mode" :color red :quit-key "q")
   ("Navigation"
    (("m" web-mode-navigate "navigate")
     ("h" web-mode-element-beginning "beginning")
     (";" web-mode-element-end "end")
     ("k" web-mode-element-next "next")
     ("i" web-mode-element-previous "previous")
     ("l" web-mode-element-child "child")
     ("h" web-mode-element-parent "parent"))
    "Editing"
    (("w" web-mode-element-wrap "wrap" :color blue)
     ("c" web-mode-element-clone "clone" :color blue)
     ("u" web-mode-element-insert "insert" :color blue)
     ("r" web-mode-element-rename "rename" :color blue)
     ("s" web-mode-element-select "select" :color blue)
     ("t" web-mode-element-transpose "transpose" :color blue)
     ("v" web-mode-element-vanish "vanish" :color blue)))))

;; configure CSS mode company backends
(use-package css-mode
  :mode "\\.css\\'"
  :mode "\\.rasi\\'"
  :config
  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-css company-dabbrev-code company-files))))
  (add-hook 'css-mode-hook 'my-css-mode-hook)
  (add-hook 'css-mode-hook 'company-mode))

;; impatient mode - Live refresh of web pages
;; https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :diminish (impatient-mode . " i")
  :commands (impatient-mode))

(provide 'lang-web)
