;;; save-check.el - Perform syntax checks AFTER saving files



;; The default linters that we ship with.
(defvar save-check-config
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
         :cond (executable-find "tflint"))

        (:mode yaml-mode
         :exec "sysbox validate-yaml %f"
         :cond (executable-find "sysbox"))
        )
      "This is the configuration for `save-check'.

Each time a file is saved it's mode will be compared with each item in this list.
If the mode matches either the command specified in `:exec' will be executed, or the
lisp expression specified in `:eval' will be evaluated.")


(defvar save-check-show-eval
  nil
  "If this variable is set then we show the output of lisp expressions evaluated with the `:eval' key.

Otherwise they are discarded.")


(defvar save-check-buffer-name
  "*save-check*"
  "The name of the buffer which is used to show failing results, if any.")





(defun save-check-config-ok-p ()
  "Test that the configuration list `save-check-config' is well-formed.

For each entry in the list we ensure there is a `mode' key, along with
either `:exec' or `:eval'.

If either sanity-check fails then an error is raised."
  (mapc #'(lambda (entry)
            (let ((exec (plist-get entry :exec))
                  (lisp (plist-get entry :eval))
                  (mode (plist-get entry :mode))
                  )

              ;; missing mode? error
              (if mode
                  ""
                (error "entry in save-check-config is missing :mode key"))

              ;; missing :exec and :lisp?  error
              (if (or exec lisp)
                  ""
                 (error "save-check config is missing :exec and :eval for mode %s" mode))))
        save-check-config)
  t)


(defun save-check()
  "The `save-check' function is added to the global `after-save-hook' when
`global-save-check-mode' is enabled.

If the current `major-mode' matches an entry in `save-check-config' then
a check will be carried out.

   A check is typically called by executing an external process, and testing
the status code of that call, however it is also possible to call an arbitrary
lisp expression.

  If a command is listed in the `:exec' key it will be invoked via `save-check-run-command',
if a lisp expression is given under the `:eval' key it will be invoked by `save-check-run-lisp'."
  (interactive)

  ;; Test the config is OK before doing anything else
  (save-check-config-ok-p)

  (mapc #'(lambda (entry)
            (let ((exec (plist-get entry :exec))
                  (cnd (plist-get entry :cond))
                  (mode (plist-get entry :mode))
                  (lisp (plist-get entry :eval))
                  (path (plist-get entry :path))
                  (run nil))

              ;; If there is a condition set
              (if cnd
                  (if (eval cnd)
                      (setq run t))   ; we run only if that passed
                (setq run t) ;; otherwise, no condition set, we run
                )

              ;; If we're to run, and the mode is either a) the exact mode, or b) derived from that mode
              ;; then do so.
              ;; If we have a lisp expression, run that.
              ;; If we have a command, run that.
              ;; This allows both, or neither, to operate
              (if (and run (or (derived-mode-p mode) (eq major-mode  mode)))
                  (progn
                    (if lisp (save-check-run-lisp lisp))
                    (if exec (save-check-run-command exec))))))
        save-check-config)
  )



(defun save-check-run-lisp(expr)
  "Execute the specified expression.

If the variable `save-check-show-eval' then the output of the specified expression will be shown in a popup-buffer, unless it is nil.  Otherwise any output will be discarded."

  ;; get, and kill, any existing buffer.
  (with-current-buffer (get-buffer-create save-check-buffer-name)
    (kill-buffer))

  ;; setup variables
  (let ((buffer (get-buffer-create save-check-buffer-name))
        (ret    nil))

    ;; evaluate the expression, and get the result
    (setq ret (eval expr))

    ;; if there was no output, kill it
    (if (not ret)
        (kill-buffer buffer)
      (if save-check-show-eval
        ;; ok output was made, show it.
        (progn
          (pop-to-buffer buffer)
          (insert ret)
          (special-mode)
        )))))



(defun save-check-run-command(cmd)
  "Execute the specified command, expanding parameters as expected:

`%f' will be replaced with the path to the file.
`%d' will be replaced with the directory containing the file.

If the command exits with a zero-return code then nothing happens, otherwise the output will be shown in a popup-buffer."

  ;; get, and kill, any existing buffer.
  (with-current-buffer (get-buffer-create save-check-buffer-name)
    (kill-buffer))

  ;; setup variables
  (let ((buffer (get-buffer-create save-check-buffer-name))
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


;; Define a new mode.
(define-minor-mode global-save-check-mode
  "This mode toggles the installation and usage of `save-check' when
files are saved."
  nil
  :global t
  :lighter " save-check"

  (if global-save-check-mode
      (add-hook 'after-save-hook #'save-check)
    (remove-hook 'after-save-hook #'save-check)))


;; End
(provide 'save-check)
