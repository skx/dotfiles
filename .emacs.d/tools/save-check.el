;;; save-check.el - Perform syntax checks AFTER saving files



;; The configuration we support
(setq save-check-config
      '(
        (yaml-mode  . "sysbox validate-yaml %s")
        (xml-mode   . "sysbox validate-xml %s")
        (json-mode  . "sysbox validate-json %s")
        (cperl-mode . "perl -wc -I. %s")
       ))


(defun save-check-run()
  "Run the save-check if the current buffer has a configured command.

The commands are contained within the list `save-check-config', and those
commands will have '%s' replaced with the path to the saved file."
  (interactive)
  (mapc #'(lambda (entry)
            (if (or (derived-mode-p (car entry)) (eq major-mode (car entry)))
                (save-check-run-command (cdr entry))))
        save-check-config)
  )


(defun save-check-run-command(cmd)
  "Execute the specified command, with the name of the buffers file as an argument.

If the command exists with a zero-return code then nothing happens, otherwise the output will be shown."
  (interactive)
  (let ((buffer (get-buffer-create "*save-check*"))
        (ret nil))

    ;; empty the buffer
    (with-current-buffer buffer
                 (erase-buffer))

    ;; call the process
    (setq ret (call-process-shell-command (format cmd buffer-file-name) nil buffer nil))
  (if (= 0 ret)
      (kill-buffer buffer)
    (pop-to-buffer buffer))))


;; Add the hook
(add-hook 'after-save-hook 'save-check-run)

;;
;; End
(provide 'save-check)
