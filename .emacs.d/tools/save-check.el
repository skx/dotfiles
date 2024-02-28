;;; save-check.el - Perform syntax checks AFTER saving files



;; The configuration we support
(setq save-check-config
      '(
        (:mode json-mode
         :exec "sysbox validate-json %s"
         :cond (executable-find "sysbox"))

        ;; This avoids creating .pyc files, which would happen if we had
        ;; used the more natural/obvious "python3 -m py_compile %s" approach
        (:mode python-mode
         :exec "python3 -c 'import ast; ast.parse(open(\"%s\").read())'"
         :cond (executable-find "python3"))

        (:mode yaml-mode
         :exec "sysbox validate-yaml %s"
         :cond (executable-find "sysbox"))

        (:mode nxml-mode
         :exec "sysbox validate-xml %s"
         :cond (executable-find "sysbox"))
       ))


(defun save-check-run()
  "Run the save-check if the current buffer has a configured command.

The commands are contained within the list `save-check-config', and those
commands will have '%s' replaced with the path to the saved file."
  (interactive)
  (mapc #'(lambda (entry)
            (let ((exec (plist-get entry :exec))
                  (cnd (plist-get entry :cond))
                  (mode (plist-get entry :mode))
                  (run nil))

              ;; If there is a condition set
              (if cnd
                  (if (eval cnd)
                      (setq run t))   ; we run only if that passed
                (setq run t) ;; otherwise, no condition set, we run
                )

              (if (and run (or (derived-mode-p mode) (eq major-mode  mode)))
                  (save-check-run-command exec))))
        save-check-config)
  )


(defun save-check-run-command(cmd)
  "Execute the specified command, with the name of the buffers file as an argument.

If the command exists with a zero-return code then nothing happens, otherwise the output will be shown."
  (interactive)
  (with-current-buffer (get-buffer-create "*save-check*")
    (kill-buffer))
  (let ((buffer (get-buffer-create "*save-check*"))
        (ret nil))

    ;; empty the buffer
    (with-current-buffer buffer
                 (erase-buffer))

    ;; call the process
    (setq ret (call-process-shell-command (format cmd buffer-file-name) nil buffer nil))
  (if (= 0 ret)
      (kill-buffer buffer)
    (progn
      (pop-to-buffer buffer)
      (special-mode)
      ))))


;; Add the hook
(add-hook 'after-save-hook 'save-check-run)

;;
;; End
(provide 'save-check)
