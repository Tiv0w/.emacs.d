;;; elisp/modules/t--treesit.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This configures treesit correctly.

(use-package treesit
  :ensure nil
  :disabled
  :config
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (c "https://github.com/tree-sitter/tree-sitter-c")
	  (clojure "https://github.com/sogaiu/tree-sitter-clojure")
	  (cmake "https://github.com/uyha/tree-sitter-cmake")
	  ;; (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
	  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (csv . ("https://github.com/amaanq/tree-sitter-csv" "master" "csv/src"))
	  ;; (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
	  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	  (elm "https://github.com/elm-tooling/tree-sitter-elm")
	  (graphql "https://github.com/bkegley/tree-sitter-graphql")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  ;; (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
	  (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  ;; (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (julia "https://github.com/tree-sitter/tree-sitter-julia")
	  (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
	  (latex "https://github.com/latex-lsp/tree-sitter-latex")
	  (lua "https://github.com/Azganoth/tree-sitter-lua")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (r "https://github.com/r-lib/tree-sitter-r")
	  (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	  (rust "https://github.com/tree-sitter/tree-sitter-rust")
	  (scala "https://github.com/tree-sitter/tree-sitter-scala")
	  (sql "https://github.com/DerekStride/tree-sitter-sql")
	  (svelte "https://github.com/Himujjal/tree-sitter-svelte")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	  (vue "https://github.com/ikatyang/tree-sitter-vue")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(provide 't--treesit)
