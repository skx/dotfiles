;;; markdown-cleanup.el - Cleanup Spacing Between Markdown headers


;; This package is designed to ensure that documents have a consistent number of
;; blank lines between markdown headers.
;;
;; This is updated to work with org-mode files too, but the naming is wrong.
;;
;;     TODO: Better name
;;
;;     TODO: Make more configurable.
;;

(defun count-chars (sep str)
  "Helper function to count how many times the given character occurs in the specified string."
   (let ((qsep (regexp-quote sep)))
     (- (length (split-string str qsep)) 1)))

(defun markdown-cleanup-header-regexp ()
  "Return the appropriate regexp for finding headers.

This supports `markdown-mode', and `org-mode', along with anything derived from either."
  (interactive)
  (cond
   ((derived-mode-p 'markdown-mode) "^\n*#+")
   ((derived-mode-p 'org-mode)      "^\n*\\*+")))

(defun markdown-cleanup-get-header-level (header)
  "Return the header depth level.

This supports `markdown-mode', and `org-mode', along with anything derived from either."
  (interactive)
  (cond
   ((derived-mode-p 'markdown-mode) (count-chars "#" header))
   ((derived-mode-p 'org-mode)      (count-chars "*" header))))


(defun markdown-cleanup-get-newlines (level)
  "Return the string of newlines for the given mode.

org-mode and org-diary mode will always return zero.  markdown will return 4-1"
  (if (derived-mode-p 'markdown-mode)
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
      ;; get the header, and count the depth of it.
      (let* ((match (match-string 0))
             (lines (count-chars "\n" match)))
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
    ;; for each header
    (while (re-search-forward (markdown-cleanup-header-regexp) nil t)
      ;; count the depth of it
      (let* ((match (match-string 0))
             (level (markdown-cleanup-get-header-level match)))
        ;; move to start of line
        (beginning-of-line)

        ;; insert newlines, based on header level
        (markdown-cleanup-get-newlines level)

        ;; forward to avoid infinite loops
        (end-of-line))))
    (markdown-cleanup-kill-leading-whitespace))


(defun markdown-cleanup-kill-leading-whitespace ()
  "Remove whitespace at the start of the file"
  (save-excursion
    (outline-show-all)
    (goto-char (point-min))
    (delete-region (point)
                   (+ (save-excursion (skip-chars-forward " \n"))
                      (point)))))

;; Install this package automatically
(add-hook 'markdown-mode-hook (lambda () (add-hook 'before-save-hook #'markdown-cleanup t t)))

;; Provide the package
(provide 'markdown-cleanup)
