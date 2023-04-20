;;; vertical-space-cleanup.el - Cleanup Spacing Between Markdown/Org headers


;; This package is designed to ensure that documents have a consistent number of
;; blank lines between headers, for both markdown files and org-mode files.
;;
;; For example this might be a Markdown file:
;;
;;  /
;;  |  # Intro
;;  |  This is my introduction
;;  |  ## Authors
;;  |  Steve
;;  |
;;  |  ## References
;;  \
;;
;; Here we see there is a difference in the vertical whitespace between the
;; two second-level headers "## Authors" and "## References".  Using this
;; package the `vertical-space-cleanup' command can be invoked and will
;; reformat all headers to have an identical number of newlines between
;; the header and any previous content.
;;
;; The configuration of the spacing depends upon the header-level.  In my
;; personal configuration:
;;
;;  # Level 1 heading - four newlines before it
;;  ## Level 2 heading  - three newlines before it
;;  ### Level 3 heading  - two newlines before it
;;  ### Level 4 heading  - one newlines before it
;;
;; The choice is made by looking at the Nth element of the list with name
;; `vertical-space-cleanup-md' - similar list exists for org-mode - after
;; removing one.
;;
;; BUGS:
;;
;; - None known, please report if you see something wrong.
;;



;; There is only one configuration value supported, which is the mapping
;; of the header-level to the number of newlines to add.
;;
;; The following two lists handle that.   We should probably use an alist
;; to get the values.
;;
(defvar vertical-space-cleanup-md
  (list
   "\n\n\n\n"  ;; level 1 - 4 newlines before it
   "\n\n\n"    ;; level 2 - 3 newlines before it
   "\n\n"      ;; level 3 - 2 newlines before it
   "\n"        ;; level 4 - 1 newlines before it
   ))          ;; other levels get zero.

(defvar vertical-space-cleanup-org
  (list
   "\n"    ;; level 1 - 1 newline
   ))      ;; other levels get zero.


(defun count-chars (sep str)
  "Helper function to count how many times the given character occurs in the specified string."
   (let ((qsep (regexp-quote sep)))
     (- (length (split-string str qsep)) 1)))

(defun vertical-space-cleanup-header-regexp ()
  "Return the appropriate regexp for finding headers.

This supports `markdown-mode', and `org-mode', along with anything derived from either."
  (cond
   ((derived-mode-p 'markdown-mode) "^\n*#+")
   ((derived-mode-p 'org-mode)      "^\n*\\*+")))

(defun vertical-space-cleanup-get-header-level (header)
  "Return the header depth level.

This supports `markdown-mode', and `org-mode', along with anything derived from either."
  (interactive)
  (cond
   ((derived-mode-p 'markdown-mode)   (count-chars "#" header))
   ((derived-mode-p 'org-mode)        (count-chars "*" header))))

(defun vertical-space-cleanup-get-newlines (level)
  "Return the string of newlines for the given mode.

This is based upon the header level, and is configurable via the two lists
`vertical-space-cleanup-md' and `vertical-space-cleanup-org'."
  (cond
   ((derived-mode-p 'markdown-mode) (nth (- level 1)   vertical-space-cleanup-md))
   ((derived-mode-p 'org-mode)      (nth (- level 1)   vertical-space-cleanup-org))))



(defun vertical-space-cleanup ()
  "Ensure there is consistent whitespace between headers and previous sections.

We iterate over the buffer and start by removing all newlines between headers,
then we iterate again and add a specific number of newlines between headers
based on the level of indentation.

This function will operate upon either `markdown-mode' or `org-mode', as well as
any modes derived from either of those."
  (interactive)
  (save-excursion
    (outline-show-all)
    (goto-char (point-min))

    ;; For each header
    (while (re-search-forward (vertical-space-cleanup-header-regexp) nil t)
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
    (while (re-search-forward (vertical-space-cleanup-header-regexp) nil t)
      ;; count the depth of it
      (let* ((match (match-string 0))
             (level (vertical-space-cleanup-get-header-level match)))
        ;; move to start of line
        (beginning-of-line)

        ;; insert newlines, based on header level
        (if (vertical-space-cleanup-get-newlines level)
            (insert (vertical-space-cleanup-get-newlines level)))

        ;; forward to avoid infinite loops
        (end-of-line))))
    (vertical-space-cleanup-kill-leading-whitespace))


(defun vertical-space-cleanup-kill-leading-whitespace ()
  "Remove whitespace at the start of the file.

This is largely required because the markdown setup will add four newlines before
a level-one header, by default, and those typically occur at the start of the documents.
"
  (save-excursion
    (outline-show-all)
    (goto-char (point-min))
    (delete-region (point)
                   (+ (save-excursion (skip-chars-forward " \n"))
                      (point)))))

;; Provide the package
(provide 'markdown-cleanup)
