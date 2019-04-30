;; Add your custom functions here

;; (defun something
;;    (do-something))

;; function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(provide 'base-functions)
