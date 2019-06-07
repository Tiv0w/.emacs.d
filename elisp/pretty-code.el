(require 'prettify-utils)

(defun my-js-and-vue-symbols-alist ()
    (setq prettify-symbols-alist
          (prettify-utils-generate
           ("() =>" "λ")
           ("function" "ƒ")
           ("null" "∅")
           ("true" "𝕋")
           ("false" "𝔽")
           ("!" "￢")
           ("&&" "∧")
           ("||" "∨")
           ("for" "∀")
           ("return" "⟼")
           ("import" "⟻"))
	  web-mode-prettify-symbols-alist prettify-symbols-alist)
    ;; (setq-default web-mode-prettify-symbols-alist
    ;;       (prettify-utils-generate
    ;;        ("() =>" "λ")
    ;;        ("function" "ƒ")
    ;;        ("null" "∅")
    ;;        ("true" "𝕋")
    ;;        ("false" "𝔽")
    ;;        ("!" "￢")
    ;;        ("&&" "∧")
    ;;        ("||" "∨")
    ;;        ("for" "∀")
    ;;        ("return" "⟼")
    ;;        ("import" "⟻")))
    )

(add-hook 'web-mode-hook 'my-js-and-vue-symbols-alist nil t)
(add-hook 'js2-mode-hook 'my-js-and-vue-symbols-alist nil t)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'my-js-and-vue-symbols-alist)

(provide 'pretty-code)
