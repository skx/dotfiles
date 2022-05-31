(defun cw_save_check ()
  "Run the Perl syntax checker on this buffer after saving."
  (cond
   ((equal mode-name "CPerl")
    (progn
      (save-current-buffer
        (set-buffer (get-buffer-create "*Perl cw output*"))
        (erase-buffer))
      (call-process-region (point-min) (point-max) "/usr/bin/perl" nil "*Perl cw output*" nil "-c -I.")
      (save-current-buffer
        (set-buffer "*Perl cw output*")
        (cond
         ((equal (buffer-string) "- syntax OK\n") (message "%s" "Syntax OK"))
         (t (message "%s" "The program has errors."))))))
   (t nil)))

(add-hook 'after-save-hook 'cw_save_check)

(provide 'perl-syntax-check)
