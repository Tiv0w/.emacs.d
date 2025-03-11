;;; lang-sql.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; SQL configuration

;;; Code:

(use-package sql-mode
  :ensure nil
  :hook
  (sql-mode . apheleia-mode)
  (sql-mode . tree-sitter-hl-mode)
  (sql-mode . lsp-deferred))


(provide 'lang-sql)
;;; lang-sql.el ends here
