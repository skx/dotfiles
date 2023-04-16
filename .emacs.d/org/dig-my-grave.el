;;; dig-my-grave.el -- Three graves turn into a special-block in org

(defun dig-my-grave ()
  "Three consecutive graves (e.g. “`”) at the start of the line prompts for
 inserting content.  See `dig-my-grave/templates-alist/org-mode'."
  (interactive)
  (if (or (and (> (point) 3)
            (string= (buffer-substring-no-properties
                       (- (point) 3) (point)) "\n``"))
        ;; Account for starting on the first line
        (and (= (point) 3)
          (string= (buffer-substring-no-properties
                     (- (point) 2) (point)) "``")))
    ;; We have just hit our third back-tick at the beginning of the line.
    (progn
      (delete-char -2)
      ;; I use the alist-get pattern a lot...perhaps a function?
      (let ((value (alist-get (completing-read "Special Content: "
                                  dig-my-grave/templates-alist/org-mode nil t)
                     dig-my-grave/templates-alist/org-mode nil nil #'string=)))
        (cond
          ;; Let's assume that we're dealing with registered org blocks.
          ((stringp value) (insert value) (forward-line -1))
          ;; Trust the function
          ((commandp value) (call-interactively value))
          ((functionp value) (funcall value))
          ((ad-lambda-p) (funcall value))
          ;; Time for a pull request
          (t (error "Unprocessable value %s for #'dig-my-grave" value)))))
    (setq last-command-event ?`)
    (call-interactively #'org-self-insert-command)))

(require 'org)
(define-key org-mode-map (kbd "`") #'dig-my-grave)


(defvar dig-my-grave/templates-alist/org-mode
  '(("bash" . "#+BEGIN_SRC bash\n#+END_SRC")
    ("shell" . "#+BEGIN_SRC sh\n#+END_SRC")
    ("example" . "#+BEGIN_EXAMPLE\n#+END_EXAMPLE")
    ("lisp" . "#+BEGIN_SRC emacs-lisp\n#+END_SRC")
  "A list of `cons' cells with `car' as the label and `cdr' as
 the value that we'll insert.  Used as the collection for the
 `dig-my-grave' `completing-read'."))

(provide 'dig-my-grave)
