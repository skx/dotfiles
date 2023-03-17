;;; perl-utilities.el - Utilities for syntax checking / formatting Perl

;;
;; Copyright (C) 2023 Steve Kemp
;;
;; Version: 0.1
;; Keywords: perl, perltidy, cperl
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

;;;
;;


(defun perl-utilities-reformat ()
  "Tidy the contents of the current buffer via `perltidy'."
  (interactive)
  (message "perltidy")
  (setq temp-point (point))
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "perltidy"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Error Buffer*"
   ;; show error buffer?
   t)
  (goto-char temp-point))


(defun perl-utilities-run-syntax-check ()
  "Run the Perl syntax checker on the current buffer."
  (setq source-full-path (buffer-file-name))
  (set-buffer (get-buffer-create "perlsyn"))
  (erase-buffer)
  (call-process "perl" nil "perlsyn" nil "-wc -I." source-full-path)
  (if (not (equal (substring (buffer-string) -3 -1) "OK"))
      (switch-to-buffer "perlsyn")
    (progn
      (kill-buffer "perlsyn")
      (message "Syntax check OK"))))



;; Call our actions when we're saving Perl buffers only.
(add-hook 'cperl-mode-hook
          (lambda ()

            ; tidy if we can
            (if (executable-find "perltidy")
                (add-hook 'before-save-hook #'perl-utilities-reformat nil 'local)
              (message "perltidy not present"))

            ; but always syntax-check
            (add-hook 'after-save-hook  #'perl-utilities-run-syntax-check nil 'local)
            ))


;; all done
(provide 'perl-utilities)
