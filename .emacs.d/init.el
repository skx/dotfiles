;;
;; ~/.emacs.d/init.el
;;
;; This is the driver which attempts to load, parse, and evaluate the
;; content of my main Markdown init-file "init.md" from beneath ~/.emacs.d/
;;
;; In addition to loading `init.md` there are a few org-mode files which
;; are also evaluated, if they're present.
;;



;; We load `org-mode` files, sometimes.
(require 'org)

;; Helper function.
(defun hostname()
  "Return the (unqualified) hostname for this system."
  (car (split-string (system-name) "\\." )))

(defun load-markdown-init (path)
  "If the specified file exists then attempt to load it.

We treat the file as markdown, and we extract code-blocks from it which we
execute directly.
"
  ;; if the file exists
  (if (file-exists-p (expand-file-name path))
      (with-temp-buffer
        ;; insert into temporary buffer
        (insert-file-contents (expand-file-name path))

        ;; Go to the start of the file.
        (goto-char (point-min))

        ;; Until we're at the end of the file ...
        (while (not (eobp))

          ;; Jump forward a line.
          (forward-line 1)

          ;; Skip to the start of the code block.
          (re-search-forward "^```lisp$" (point-max) t)

          (let ((l (match-end 0)))
            ;; Find the end of the code-block
            (re-search-forward "^```$" (point-max) t)

            ;; Evaluate it, and then keep going.
            (if (< l (match-beginning 0))
                (eval-region l (match-beginning 0))))))
    (message "Skipping file that doesn't exist %s" path)))


;; Load the global-file markdown file.
;;
;; We never supported any per-host, or local markdown files, so this is
;; simple and unconditional.
(load-markdown-init "~/.emacs.d/init.md")
