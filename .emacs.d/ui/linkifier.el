;;; linkifier.el --- Convert text into clickable links, by regexp

;;
;; Copyright (C) 2023 Steve Kemp
;;
;; Version: 0.1
;; Keywords: links, web, utility, regexp
;; Author: Steve Kemp <steve@steve.fi>
;;

;; This file is not (YET) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;; Setup the regexp to destination mappings.
;;
;; Note that "\<XXX\>" means that we only match XXX when not surrounded
;; by "word constituent" characters - we could also use \b for the same
;; purpose.
(setq linkifier-patterns '(
                           ("\\\<XXX-[0-9]+\\\>" "https://jira.example.com/browse/%s")
                           ("\\\<BUG-[0-9]+\\\>" "https://bugzilla.example.com/show?id=%s")))

;; Ensure we have a type for our buttons.
(define-button-type 'linkifier)

(defun linkifier-insert-buttons (beg end)
  "Insert buttons within the current buffer, between the specified start and end.

   What this does is iterate over the regexp&URL pairs in `linkifier-patterns` and
  replace any text that matches the regular expression specified with a button, which
  will open the destination URL when selected."
  (remove-overlays beg end 'type 'linkifier)
  (loop for (regexp . destination) in linkifier-patterns do
        (save-excursion
          (goto-char beg)
          (while (re-search-forward regexp end t)
            (make-button (match-beginning 0)
                         (match-end 0)
                         'dest destination
                         'type 'linkifier
                         'action `(lambda (button)
                                    (browse-url (format (car (button-get button 'dest)) (button-get button 'text))))
                         'follow-link t
                         'text (match-string 0))))))

(define-minor-mode linkifier-mode nil nil nil nil
  "`linkifier-mode` is a simple minor mode for turning text matching a
 series of regular expressions into buttons/hyperlinks with templated
 destinations.

  It can be handy for converting MP-XXX into a hyperlink to a Jira
 installation, bugzilla instance, or similar."
  (cond
   (linkifier-mode
    (jit-lock-register #'linkifier-insert-buttons)
    (linkifier-insert-buttons (point-min) (point-max)))
   (t
    (jit-lock-unregister #'linkifier-insert-buttons)
    (remove-overlays (point-min) (point-max) 'type 'linkifier))))


(provide 'linkifier)
