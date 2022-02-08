;; org-tag-cloud.el - Maintain a tag-cloud inside an org-mode file
;;
;;

(defun org-tag-cloud-insert ()
  "Insert a block which will be used to generate a tag-contents at point.

The block can be executed via org-tag-cloud-update."
  (interactive "*")
  (insert "
#+NAME: org-tag-cloud
#+BEGIN_SRC emacs-lisp :colnames '(Frequency Tag) :exports results
(org-tag-cloud-populate)
#+END_SRC
"))

(defun org-tag-cloud-update ()
  "Update the existing tag-cloud, by executing the block named 'org-tag-cloud'"
  (interactive "*")
  (save-excursion
    (org-save-outline-visibility t
      (if (member "org-tag-cloud" (org-babel-src-block-names))
          (progn
            (setq-local org-confirm-babel-evaluate nil)
            (org-babel-goto-named-src-block "org-tag-cloud")
            (org-babel-execute-src-block))
        (message "There is no block named 'org-tag-cloud' present")))))

(defun org-tag-cloud-populate ()
  "Return a suitable structure which can be used to generate a tag-cloud."
  (let (tags count)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-complex-heading-regexp nil t)
        (dolist (tag (org-get-tags))
          (unless (equalp tag "")
            (push tag tags))))
      (cl-loop with result
               for tag in tags
               do (push (list (cl-count tag tags
                                        :test #'string=)
                              (format "[[elisp:(org-tags-view nil \"%s\")][%s]]" tag tag))
                        count)
               collect
               (setq result (cl-remove-duplicates count :test #'equal))
               finally return
               (cl-sort result #'> :key #'car)))))


(provide 'org-tag-cloud)
