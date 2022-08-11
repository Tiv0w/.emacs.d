;;; elisp/lang/lang-javascript.el -*- lexical-binding: t; -*-

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :bind (:map js2-mode-map
              (("C-x C-e" . js-send-last-sexp)
               ("C-M-x" . js-send-last-sexp-and-go)
               ("C-c C-b" . js-send-buffer-and-go)
               ("C-c C-l" . js-load-file-and-go)))
  :mode
  ("\\.js$" . js2-mode)
  ;; ("\\.json$" . js2-jsx-mode)
  :config
  (custom-set-variables '(js2-strict-inconsistent-return-warning nil))
  (custom-set-variables '(js2-strict-missing-semi-warning nil))

  (setq js-indent-level 4)
  (setq js2-indent-level 4)
  (setq js2-basic-offset 4)
  :mode-hydra
  ((:title "JavaScript-mode" :color blue :quit-key "q")
   ("Navigation"
    (("m" xref-find-definitions "jump to def")
     ("," xref-pop-marker-stack "jump back")
     ("." xref-find-references "jump to impl"))
    "Editing"
    (("s" js2r-rename-var "rename symbol")
     ("a" crux-rename-file-and-buffer "rename file")
     ("d" js-doc-insert-function-doc "jsdoc")
     ("l" js2r-log-this "log this")))))

;; Run a JavaScript interpreter in an inferior process window
;; https://github.com/redguardtoo/js-comint
(use-package js-comint
  :after js2-mode
  :config
  (setq inferior-js-program-command "node"))

;; js2-refactor :- refactoring options for emacs
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :after js2-mode
  ;; :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c j r")
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

(use-package xref-js2
  :config
  (setq xref-js2-search-program 'rg)
  (message "xref-js2: DON'T FORGET TO UPSTREAM THE FIX (file-remote-p support)")
  (defun xref-js2--find-candidates (symbol regexp)
    (let ((default-directory (xref-js2--root-dir))
          matches)
      (with-temp-buffer
        (let* ((search-tuple (cond ;; => (prog-name . function-to-get-args)
                              ((eq xref-js2-search-program 'rg)
                               '("rg" . xref-js2--search-rg-get-args))
                              (t ;; (eq xref-js2-search-program 'ag)
                               '("ag" . xref-js2--search-ag-get-args))))
               (search-program (car search-tuple))
               (search-args    (remove nil ;; rm in case no search args given
                                       (funcall (cdr search-tuple) regexp))))
          (apply #'process-file (executable-find search-program (file-remote-p default-directory)) nil t nil search-args))

        (goto-char (point-max)) ;; NOTE maybe redundant
        (while (re-search-backward "^\\(.+\\)$" nil t)
          (push (match-string-no-properties 1) matches)))
      (seq-remove #'xref-js2--false-positive
                  (seq-map (lambda (match)
                             (xref-js2--candidate symbol match))
                           matches))))

  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package add-node-modules-path
  :hook js2-mode)

(use-package flycheck
  :hook (js2-mode . flycheck-mode)
  :after (js2-mode add-node-modules-path)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (if (executable-find "eslint")
      (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-select-checker 'javascript-eslint))
  (flycheck-mode))

;; (use-package tide
;;   :ensure t
;;   :after (js2-mode company flycheck add-node-modules-path)
;;   :hook ((js2-mode . tide-setup)
;;       (js2-mode . tide-hl-identifier-mode)
;;       (before-save . tide-format-before-save))
;;   :config
;;   (eldoc-mode +1)
;;   (setq
;;    tide-native-json-parsing t
;;    tide-completion-detailed t
;;    tide-always-show-documentation t
;;    ;; By default, tide ignores payloads larger than 100kb. This is too small for
;;    ;; larger projects that produce long completion lists, so we up it to 512kb.
;;    tide-server-max-response-length 524288 )

;;   ;; ;; code completion
;;   ;; (after! company
;;   ;;   ;; tide affects the global `company-backends', undo this so doom can handle
;;   ;;   ;; it buffer-locally
;;   ;;   (setq-default company-backends (delq 'company-tide (default-value 'company-backends))))
;;   ;; (set-company-backend! 'tide-mode 'company-tide)
;;   ;; ;; navigation
;;   ;; (set-lookup-handlers! 'tide-mode :async t
;;   ;;   :xref-backend #'xref-tide-xref-backend
;;   ;;   :documentation #'tide-documentation-at-point)
;;   ;; (set-popup-rule! "^\\*tide-documentation" :quit t)
;;   ;; ;; resolve to `doom-project-root' if `tide-project-root' fails
;;   ;; (advice-add #'tide-project-root :override #'+javascript-tide-project-root-a)
;;   ;; ;; cleanup tsserver when no tide buffers are left
;;   ;; (add-hook! 'tide-mode-hook
;;   ;;   (add-hook 'kill-buffer-hook #'+javascript-cleanup-tide-processes-h nil t))

;;   ;; Eldoc is activated too soon and disables itself, thinking there is no eldoc
;;   ;; support in the current buffer, so we must re-enable it later once eldoc
;;   ;; support exists. It is set *after* tide-mode is enabled, so enabling it on
;;   ;; `tide-mode-hook' is too early, so...
;;   ;; (advice-add #'tide-setup :after #'eldoc-mode
;;   )


(set-pretty-symbols! '(js2-mode rjsx-mode web-mode js-mode)
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

(use-package eslintd-fix
  :hook (js2-mode . eslintd-fix-mode))

;; (require 'lang-javascript-helper)

(provide 'lang-javascript)
