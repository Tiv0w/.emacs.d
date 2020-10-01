;;; elisp/modules/t--pdf.el -*- lexical-binding: t; -*-
;;; Commentary:
;; PDF utils.

;;; Code:

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(use-package org-noter
  :disabled
  :config
  (setq org-noter-auto-save-last-location t
        org-noter-separate-notes-from-heading t))


(provide 't--pdf)
