;;; elisp/lang/lang-vue.el -*- lexical-binding: t; -*-

;;; Code:
(use-package web-mode)

(define-derived-mode vue-mode web-mode "Vue"
  "Major mode for Vue.js files.  Derived from web-mode.")

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook #'vue-setup-editor-style)

(defun vue-setup-editor-style ()
  (setq web-mode-script-padding 0
        web-mode-style-padding 0
        web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-closing t)
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal) "//"))

(major-mode-hydra-define vue-mode
  (:title "Vue" :color blue :quit-key "q")
  ("Navigation"
   (("m" lsp-find-definition "jump to def")
    ("," lsp-find-references "jump back")
    ("." lsp-find-implementation "jump to impl")
    ("n" web-mode-navigate "navigate")
    ("h" web-mode-element-beginning "beginning" :color red)
    (";" web-mode-element-end "end" :color red)
    ("k" web-mode-element-next "next" :color red)
    ("i" web-mode-element-previous "previous" :color red)
    ("l" web-mode-element-child "child" :color red)
    ("j" web-mode-element-parent "parent" :color red))
   "Editing"
   (("ww" web-mode-element-wrap "wrap")
    ("wc" web-mode-element-clone "clone")
    ("wu" web-mode-element-insert "insert")
    ("wr" web-mode-element-rename "rename")
    ("ws" web-mode-element-select "select")
    ("wt" web-mode-element-transpose "transpose")
    ("wv" web-mode-element-vanish "vanish"))
   "Refactors"
   (("f" lsp-execute-code-action "lsp actions")
    ("r" lsp-rename "rename")
    ("R" lsp-javascript-rename-file "rename file")
    ("o" lsp-organize-imports "organize imports")
    ("b" lsp-eslint-fix-all "lint code"))))

(use-package add-node-modules-path
  :hook (vue-mode . add-node-modules-path))

(use-package emmet-mode
  :hook vue-mode
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package web-mode-edit-element
  :hook (vue-mode . web-mode-edit-element-minor-mode))

(use-package lsp-mode
  :hook (vue-mode . lsp))

(defun log-at-point ()
  "Console.logs the thing at point.
Does not take the `this' in JS, use a region for that."
  (interactive "")
  (save-excursion
    (let ((x (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (thing-at-point 'symbol))))
      (end-of-line)
      (reindent-then-newline-and-indent)
      (insert (format "console.log('%s: ', %s);" x x)))))

(set-pretty-symbols! '(vue-mode)
    ;; Functional
    :def "function"
    :lambda "() =>"
    :composition "compose"
    ;; Types
    :null "null"
    :true "true"
    :false "false"
    ;; Flow
    :not "!"
    :and "&&"
    :or "||"
    :for "for"
    :return "return"
    ;; Other
    :yield "import")

(provide 'lang-vue)
;;; lang-vue.el ends here
