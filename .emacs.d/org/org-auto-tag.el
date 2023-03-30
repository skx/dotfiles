;;; org-auto-tag.el -- Add tags to task automatically

;;
;; When this package is loaded TODO-tasks will have tags added automatically
;; if the text contains the name of a previously used tag.
;;
;; Usage:
;;
;;  1. Require the package.
;,     (require 'org-auto-tag)
;;
;;  2. Enable it in the state-change hook
;;
;;     (add-hook 'org-after-todo-state-change-hook 'org-auto-tag)
;;
;;


(defun get-all-tags ()
  "Return a list of all tags used within this file, in lowercase.

  Tags are returned in random order, however duplicates are removed."
  (let ((all-tags '()))
    (org-map-entries
      (lambda ()
        (let ((tag-string (car (last (org-heading-components)))))
          (when tag-string
           (setq all-tags (append all-tags (split-string tag-string ":" t)))))))
    (mapcar #'downcase (delete-dups all-tags))))


(defun org-auto-tag ()
  "For every word in the current headline, see if it is a valid tag-name.

  If the tag-name exists then added it as a tag to the headline."
  (interactive)
  (let ((alltags (get-all-tags))
        (headline-words (split-string (downcase (org-get-heading t t))))
       )
    (mapcar (lambda (word) (if (member word alltags)
                               (org-toggle-tag word 'on)))
            headline-words)))


(provide 'org-auto-tag)
