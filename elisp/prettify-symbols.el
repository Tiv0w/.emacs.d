(require 'prettify-utils)

(setq-default prettify-symbols-alist
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
               ("import" "⟻")))
