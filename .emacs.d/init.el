(with-temp-buffer

  ;; Load ~/.emacs.d/init.md
  (insert-file-contents (expand-file-name "init.md" user-emacs-directory))

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

      ;; Write out the region we've found for debug-purposes
;;      (append-to-file l (match-beginning 0) (expand-file-name "initel.el" user-emacs-directory))

      ;; Evaluate it, and then keep going.
      (eval-region l (match-beginning 0)))))
