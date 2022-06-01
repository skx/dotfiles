
(defun perltidy()
  "Tidy the contents of the current buffer via `perltidy'"
  (interactive)
  (message "perltidy")
  (setq temp-point (point))
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "perltidy"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Error Buffer*"
   ;; show error buffer?
   t)
  (goto-char temp-point))

; Run our function on save
(defun perl-tidy-check-hook ()
  (when (or (eq major-mode 'perl-mode)
            (eq major-mode 'cperl-mode))
    (perltidy)))

(add-hook 'before-save-hook #'perl-tidy-check-hook)

(provide 'perl-tidy-on-save)
