(defun cw_save_check ()
  "Run the Perl syntax checker on this buffer after saving."
  ;;get the full path of this file
  (setq source-full-path (buffer-file-name))
  ;;clear the buffer of earlier error
  (set-buffer (get-buffer-create "perlsyn"))
  (erase-buffer)
  ;;execute perl -wc
  (call-process "perl" nil "perlsyn" nil "-wc -I." source-full-path)
  (if (not (equal (substring (buffer-string) -3 -1) "OK"))
      (switch-to-buffer "perlsyn")
    (progn
      (kill-buffer "perlsyn")
      (message "Syntax check OK"))))


; Run our function on save
(defun perl-syntax-check-hook ()
  (when (or (eq major-mode 'perl-mode)
            (eq major-mode 'cperl-mode))
    (cw_save_check)))

(add-hook 'after-save-hook #'perl-syntax-check-hook)

(provide 'perl-syntax-check)
