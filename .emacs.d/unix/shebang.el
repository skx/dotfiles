;; -*- mode:emacs-lisp; mode: outline-minor -*-
;;
;; RCSfile: shebang.el,v
;; Revision: 1.3
;; Date: 1996/12/19 03:43:21
;;
;; NAME
;; shebang.el -- check for valid #! interpreter and set executable mode
;;
;; LICENSE
;; This source code is hereby released to the public domain and is unsupported.
;; You are encouraged to copy and modify this file.  Please clearly document
;; modifications with authorship and motivation.  Bug reports, code
;; contributions, and suggestions are appreciated.
;; 
;; SOURCE
;; New versions of this file may be obtained from (as of 1998/11/08)
;; http://www.in-machina.com/~reece/emacs/shebang.el
;; ftp://in-machina.com/pub/reece/emacs/shebang.el
;; 
;; AUTHOR
;; Reece Hart, http://www.in-machina.com/~reece/, PGP:0xD178AAF9
;; Do not send unsolicited bulk email.  Boycott companies which do so.
;; 

(defvar shebang-check-interpreter t
  "Verify that the interpreter exists.")

; if string, then assumed to be a+x 
; make this shebang-exec-modes
(defvar shebang-modes 493
  "The permissions used for executable files.  A typical value is 493
(755 octal) for rwxr-xr-x permissions.  Note that the value must be an
integer.")

(defun shebang-chmod ()
  "If file starts with `#!' then possibly chmod the file to the perms
specified by the variable shebang-modes."
  (interactive)
  (let ((truename (file-truename (buffer-file-name))))
    (and
     (save-excursion                    ; file is shebang'd
       (goto-char 1)
       (looking-at "#!"))
    (not (file-executable-p truename)) ; and not already executable
     (not (= (file-modes truename) shebang-modes))
     ;; then set mode
     (set-file-modes (file-truename (buffer-file-name)) shebang-modes)
     )
    )
  )
(add-hook 'after-save-hook 'shebang-chmod)

(defun shebang-check-interpreter ()
  (interactive)
  (save-excursion                   ; file is shebang'd
    (goto-char 1)
    (if
        (re-search-forward "^#![ ]?\\([a-zA-Z_./]+\\)" 50 t) ; int. < 50-2 chars
        (let ((interpreter (buffer-substring (match-beginning 1) (match-end 1))))
          (if (or 
                ;; doesn't begin with /
                (not (= (string-match "/" interpreter) 0))
                ;; non (existent and executable)
                (not (and (file-exists-p interpreter)
                          (file-executable-p interpreter))))
              (message "%sWarning: `%s' is not a valid interpreter.")
            )
          )
      )
    )
  )
(add-hook 'after-save-hook 'shebang-check-interpreter)

(provide 'shebang)