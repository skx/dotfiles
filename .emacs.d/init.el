;;
;; ~/.emacs.d/init.el
;;
;; This is the driver which attempts to load, parse, and evaluate the
;; content of my main Markdown init-file "init.md" from beneath ~/.emacs.d/
;;


(defun load-markdown-init (path)
  "If the specified file exists then attempt to load it.

We treat the file as markdown, and we extract code-blocks from it which we
execute directly."
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

;; Record our start time
(defconst skx/startup-begin (float-time))

;; Load the global-file markdown file.
(load-markdown-init "~/.emacs.d/init.md")

;; Record the end of our startup time
(defconst skx/startup-end (float-time))

;; Show the duration
(defconst skx/startup-duration (- skx/startup-end skx/startup-begin))
(message "[Emacs startup duration %.2f seconds]" skx/startup-duration)
