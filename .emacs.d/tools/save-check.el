;;; save-check.el - Perform syntax checks AFTER saving files

;;
;; Copyright (C) 2024 Steve Kemp
;;
;; Version: 0.1
;; Keywords: perl, dockerfile, json, python, shell, terraform, linter, xml, yaml
;; Author: Steve Kemp <steve@steve.fi>
;;

;; This file is not (YET) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;;
;;

;;
;; This package defines a standard way of running linters, automatically,
;; when files are saved.
;;
;; The intention is that we can add a global hook to be executed when files
;; are saved, and within that hook "do the right thing" - which means run the
;; appropriate linter based on the buffers's major-mode.
;;
;; Several sane linters are defined within this package, and adding new ones
;; is straightforward.
;;





;; The default linters that we are configured to run.
;;
;; Within the commands limiting expansion takes place:
;;
;;     %f is replaced with the filename to lint.
;;     %d is replaced with the directory containing the file.
;;
(defvar save-check-config
      '(

        ;; Here we see :mode is actually set to a list
        ;; so this linter will run for both modes.
        (:mode (cperl-mode perl-mode)
         :exec "perl -wc -I. %f"
         :cond (executable-find "perl"))

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

Otherwise any output, or return value, is discarded.")


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

If an external command is executed and terminates with a non-zero exit code
its output will be displayed in a buffer for reference.

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
                  (run nil)
                  (tmp nil))

              ;; Make tmp a list of the modes from the :mode parameter.
              ;;
              ;; We want to do this so we can have either of these work:
              ;;
              ;;   :mode foo-mode
              ;;   :mode (cperl-mode perl-mode)
              ;;
              ;; We want to process these identically, so we'll just
              ;; pretend we always had a list - and tmp will have that.
              ;;
              (if (listp mode)
                  (setq tmp mode)        ;; already a list
                (setq tmp (list mode)))  ;; make a list (of one element)

              ;; If there is a condition set
              (if cnd
                  (if (eval cnd)
                      (setq run t))   ; we run only if that passed
                (setq run t) ;; otherwise, no condition set, we run
                )

              ;; If we're to run, and the mode is either:
              ;;   a) the exact mode
              ;;   b) derived from that mode
              ;;
              ;; We have a list of modes, so we'll test each one, but
              ;; we'll stop after the first - by setting run to nil
              ;;
              (mapc #'(lambda (m)
                        (if (and run (or (derived-mode-p m) (eq major-mode m)))
                            (progn
                              (setq run nil)                             ;; only lint once
                              (if lisp (save-check-run-lisp lisp))       ;; call :eval
                              (if exec (save-check-run-command exec))))) ;; call :exec
                    tmp)
              )
            )
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
