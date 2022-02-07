;;; org-nested.el - Allow links to be refined by regular expression.
;;;
;;; This package allows you to link to child headers, easily, via
;;; regular expressions added to normal links.
;;;
;;; It is useful if you want to have a standard set of headers which
;;; are not unique per-day.
;;;

;; Save reference to the original function
(defvar orig--org-link-search  (symbol-function 'org-link-search))

;; Override the function
(defun org-link-search (s &optional avoid-pos stealth)
  "org-link-search is an overridden function provided by org-nested.

This overridden function allows a link to contain an optional set of
regular expressions which will be used to further move forward from
a link-target.

This is designed to allow you to jump to the _next_ heading after a
specified one.
"
  (let* ((parts (split-string s "#"))    ; Get the parts
         (first (car parts))             ; Follow the first one as normal
         (rest (cdr parts)))             ; Get any refinements

    ; Call the original function.
    (funcall orig--org-link-search first avoid-pos stealth)

    ; Now for each refinement, search for the corresponding sub-header.
    (mapc #'(lambda (thing)
              (re-search-forward (format "^\**\s*%s" thing) nil)) rest)))


; We're done here now.
(provide 'org-nested)
