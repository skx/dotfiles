;;; save-check.el - Perform syntax checks AFTER saving files



;; The default linters that we ship with.
(setq save-check-config
      '(

        (:mode dockerfile-mode
         :exec "hadolint --no-color %f"
         :cond (executable-find "hadolint"))

        (:mode json-mode
         :exec "sysbox validate-json %f"
         :cond (executable-find "sysbox"))

        (:mode nxml-mode
         :exec "sysbox validate-xml %f"
         :cond (executable-find "sysbox"))

        ;; This avoids creating .pyc files, which would happen if we had
        ;; used the more natural/obvious "python3 -m py_compile %s" approach
        (:mode python-mode
         :exec "python3 -c 'import ast; ast.parse(open(\"%f\").read())'"
         :cond (executable-find "python3"))

        (:mode sh-mode
         :exec "shellcheck %f"
         :cond (executable-find "shellcheck"))

        (:mode terraform-mode
         :exec "tflint --no-color --chdir %d"
         :cond (executable-find "tflint")
         :path t)

        (:mode yaml-mode
         :exec "sysbox validate-yaml %f"
         :cond (executable-find "sysbox"))
       ))


(defun save-check-run()
  "Run the save-check if the current buffer has a configured command.

The commands are contained within the list `save-check-config', and those
commands will have parameters replaced appropriately by `save-check-run-command'."
  (interactive)
  (mapc #'(lambda (entry)
            (let ((exec (plist-get entry :exec))
                  (cnd (plist-get entry :cond))
                  (mode (plist-get entry :mode))
                  (path (plist-get entry :path))
                  (run nil))

              ;; If there is a condition set
              (if cnd
                  (if (eval cnd)
                      (setq run t))   ; we run only if that passed
                (setq run t) ;; otherwise, no condition set, we run
                )

              ;; If we're to run, and the mode is either a) the exact mode, or b) derived from that mode
              ;; then run.
              (if (and run (or (derived-mode-p mode) (eq major-mode  mode)))
                  (save-check-run-command exec))))
        save-check-config)
  )


(defun save-check-run-command(cmd)
  "Execute the specified command, expanding parameters as expected:

`%f' will be replaced with the path to the file.
`%d' will be replaced with the directory containing the file.

If the command exists with a zero-return code then nothing happens, otherwise the output will be shown in a popup-buffer."

  ;; get, and kill, any existing buffer.
  (with-current-buffer (get-buffer-create "*save-check*")
    (kill-buffer))

  ;; setup variables
  (let ((buffer (get-buffer-create "*save-check*"))
        (exec   cmd)
        (ret    nil))

    ;; Expand the string we're to execute.
    ;;  %f -> file
    (setq exec (string-replace "%f" buffer-file-name exec))
    ;;  %d -> directory
    (setq exec (string-replace "%d" (file-name-directory buffer-file-name) exec))

    ;; call the process
    (setq ret (call-process-shell-command exec nil buffer nil))

    ;; if the return code was OK, kill the results.
    ;; otherwise pop to the buffer, and set it to be "special mode"
    ;; which has suitable keybindings.
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
