;;; org-utils.el -- A collection of org-related utility functions

;;
;; These functions were extracted from being within my init-file
;; and might be useful to share with others.
;;


(defun org-utils-header-prop( name )
  "Get the value from the global property with the given name, e.g. 'AUTHOR', 'TITLE', etc."
  (save-excursion
    (org-save-outline-visibility t
      (outline-show-all)
      (goto-line 0)
      (if (re-search-forward (concat "^#\\+" name ":") nil t)
          (progn
            (setq start (point))
            (re-search-forward "$")
            (setq end (point))
            (string-trim (buffer-substring-no-properties start end)))))))



(defun org-utils-random-heading (top-level)
  "Open random top-level heading from current Org buffer in new indirect buffer.
    When TOP-LEVEL is non-nil, only go to top-level headings."
  (interactive "P")
  (goto-char (+ (point-min) (random (- (point-max) (point-min)))))
  (if (org-before-first-heading-p)
      (outline-next-heading)
    (org-back-to-heading)
    (when top-level
      (cl-loop while (org-up-heading-safe))))
  (org-tree-to-indirect-buffer))



(defun org-utils-table-next (&optional arg)
  "Jump to the next table.

With a prefix argument ARG, jump forward ARG many tables."
  (interactive "p")
  (dotimes (n arg)
    (let ((pt (point)))
      (when (org-at-table-p)
        (goto-char (org-table-end)))
      (if (re-search-forward org-table-line-regexp nil t)
          (when (org-invisible-p)
            (org-reveal t)
            (org-show-entry)
            (unless (org-at-table-p)
              (org-utils-table-next 1)))
        (goto-char pt)))))



(defun org-utils-table-prev (&optional arg)
  "Jump to the previous table.

With a prefix argument ARG, jump backward ARG many tables."
  (interactive "p")
  (dotimes (n arg)
    (let ((pt (point)))
      (when (org-at-table-p)
        (goto-char (org-table-begin)))
      (if (re-search-backward org-table-line-regexp nil t)
          (progn
            (when (org-invisible-p)
              (org-reveal t)
              (org-show-entry)
              (unless (org-at-table-p)
                (org-utils-table-prev 1)))
            (goto-char (1+ (org-table-begin))))
        (goto-char pt)))))



(provide 'org-utils)
