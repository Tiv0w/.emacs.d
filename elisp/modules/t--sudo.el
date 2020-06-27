;;; elisp/modules/t--sudo.el -*- lexical-binding: t; -*-
;;; Commentary:
; Various functions related to sudo opening files via TRAMP
;
; All the code was taken from Doom Emacs (https://github.com/hlissner/doom-emacs)


(defun t--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun t--sudo-find-file (file)
  "Open FILE as root."
  (interactive "Open file as root: ")
  (find-file (t--sudo-file-path file)))

(defun t--sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (t--sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

(defun t--sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (t--sudo-file-path buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
              (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
              (kill-buffer buffer))
            (with-current-buffer origin
              (revert-buffer t t))))
      (user-error "Unable to open %S" file))))


(provide 't--sudo)
