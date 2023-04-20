;;; markdown-cleanup.el - Cleanup Spacing Between Markdown headers

;; This package is designed to ensure that documents have a consistent number of
;; blank lines between markdown headers.
;;
;; This is updated to work with org-mode files too, but the naming is wrong.
;;
;;     TODO: Better name
;;
;;     TODO: Use a derived mode to test for things.
;;
;;     TODO: Make more configurable.
;;

(defun count-chars (char str)
  "Helper function to count how many times the given character occurs in the specified string."
  (let ((s (char-to-string char))
        (count 0)
        (start-pos -1))
    (while (setq start-pos (string-search s str (+ 1 start-pos)))
      (setq count (+ 1 count)))
    count))

(defun markdown-cleanup-header-regexp ()
  "Return the appropriate regexp for finding headers.

This supports `markdown-mode', `org-diary-mode', and `org-mode'."
  (interactive)
  (cond
   ((eq 'markdown-mode (buffer-local-value 'major-mode (current-buffer))) "^\n*#+")
   ((eq 'org-diary-mode (buffer-local-value 'major-mode (current-buffer))) "^\n*\\*+")
   ((eq 'org-mode (buffer-local-value 'major-mode (current-buffer))) "^\n*\\*+")))

(defun markdown-cleanup-get-header-level (header)
  "Return the header depth level.

This supports `markdown-mode', `org-diary-mode', and `org-mode'."
  (interactive)
  (cond
   ((eq 'markdown-mode (buffer-local-value 'major-mode (current-buffer))) (count-chars ?# header))
   ((eq 'org-mode (buffer-local-value 'major-mode (current-buffer))) (count-chars ?* header))
   ((eq 'org-diary-mode (buffer-local-value 'major-mode (current-buffer))) (count-chars ?* header))))

(defun markdown-cleanup-get-newlines (level)
  "Return the string of newlines for the given mode.

org-mode and org-diary mode will always return zero.  markdown will return 4-1"
  (if (eq 'markdown-mode (buffer-local-value 'major-mode (current-buffer)))
      ;; markdown
      (cond ((= level 1) (insert "\n\n\n\n"))
            ((= level 2) (insert "\n\n\n"))
            ((= level 3) (insert "\n\n"))
            ((= level 4) (insert "\n"))
            (t (insert "")))
    ;; org
    (cond ((= level 1) (insert "\n"))
          (t (insert "")))))


(defun markdown-cleanup ()
  "Ensure there is consistent whitespace between headers and previous sections.

We iterate over the buffer and start by removing all newlines between markdown headers,
then we iterate again and add a specific number of newlines between headers - based on
the level of indentation."
  (interactive)
  (save-excursion
    (outline-show-all)
    (goto-char (point-min))

    ;; For each header
    (while (re-search-forward (markdown-cleanup-header-regexp) nil t)
      (let* ((match (match-string 0))
             (lines (count-chars ?\n match)))
        ;; move to start of line
        (beginning-of-line)
        ;; remove each newline
        (while (> lines 0)
          (delete-backward-char 1)
          (setq lines (- lines 1))))
      ;; advance to next line, so we don't loop forever
      (end-of-line))

    ;; Need to start at the beginning of the file again.
    (goto-char (point-min))

    ;; now add new lines
    (while (re-search-forward (markdown-cleanup-header-regexp) nil t)
      (let* ((match (match-string 0))
             (level (markdown-cleanup-get-header-level match)))
        ;; move to start of line
        (beginning-of-line)
        (message "Level: %d" level)
        ;; insert newlines, based on header level
        (markdown-cleanup-get-newlines level)

        ;; forward to avoid infinite loops
        (end-of-line))))
    (markdown-cleanup-kill-leading-whitespace))


(defun markdown-cleanup-kill-leading-whitespace ()
  "Remove whitespace at the start of the file"
  (save-excursion
    (goto-char (point-min))
    (delete-blank-lines)
    (delete-forward-char 1)))

;; Install this package automatically
(add-hook 'markdown-mode-hook (lambda () (add-hook 'before-save-hook #'markdown-cleanup t t)))

;; Provide the package
(provide 'markdown-cleanup)
