;;; elisp/lang/lang-sql.el -*- lexical-binding: t; -*-

;;; SQL configuration

(use-package sql-mode
  :ensure nil
  :hook (sql-mode . apheleia-mode))

(use-package tree-sitter
  :hook (sql-mode . tree-sitter-hl-mode))

(provide 'lang-sql)
;;; lang-sql.el ends here
