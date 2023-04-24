;;; org-nested-links.el - Allow links to be refined by regular expression.
;;;
;;; This package allows you to link to child headers, easily, via
;;; regular expressions added to normal links.
;;;
;;; It is useful if you want to have a standard set of headers which
;;; are not unique per-day.
;;;
;;; A normal link would look like this:
;;;
;;;   [[21/04/2023]]
;;;
;;; If you wanted to search for the first occurence of the word "Cake" beneath
;;; that entry you would refine your link like so:
;;;
;;;   [[21/04/2023#Cake]]
;;;
;;; What this does:
;;;
;;; - Search for the headline "21/04/2023"
;;;
;;; - Once that is found further search (forward) for "Cake"
;;;
;;; The point here is if you have a lot of Cake-related entries, and you wish
;;; to find the correct one.  (Of course using persistent UUIDs for each
;;; headline is an alternative solution which works too, more generally, but
;;; at a cost of readability.)
;;;
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

    ; expand the item so that it is visible
    (show-subtree)

    ; Now for each refinement, search for the corresponding sub-header.
    (mapc #'(lambda (thing)
              (re-search-forward thing nil)) rest)))


; We're done here now.
(provide 'org-nested-links)
