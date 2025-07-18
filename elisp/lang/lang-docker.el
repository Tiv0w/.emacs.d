;;; elisp/lang/lang-docker.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Docker setup.

;;; Code:
(use-package dockerfile-ts-mode
  :hook
  (dockerfile-ts-mode . lsp-deferred)
  (dockerfile-ts-mode . apheleia-mode))

(provide 'lang-docker)
;;; lang-docker.el ends here
