;;; skx-template.el --- insert template when visiting new files

;;
;; Copyright (C) 2020 Steve Kemp
;;
;; Version: 0.1
;; Keywords: templates
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



(defun skx-get-file-lang-mode (file)
  "Given a filename return the language-mode which will be used to handle it,
   via inspection of auto-mode-alist."
  (let* ((index 0)
	assoc)
    (setq assoc
          (catch 'match
            (while (setq assoc (nth index auto-mode-alist))
              (if (string-match (car assoc) file)
                  (throw 'match assoc)
                (setq index (1+ index))))
            (setq assoc nil)))

    (if assoc
        (format "%s" (cdr assoc))
      "")))

(defun skx-insert-template(name)
  "If there is a ~/.emacs.d/templates/ file which has the same name as the file's mode then insert it when visiting a new file."
  (let* ((mode (skx-get-file-lang-mode name)))
    (if (and (> (length mode) 0)
             (file-exists-p (expand-file-name (concat "~/.emacs.d/templates/" mode))))
        (progn
          (insert-file-contents (expand-file-name (concat "~/.emacs.d/templates/" mode)))
          (end-of-buffer)
          )
      (if (> (length mode) 0 )
          (message "skx-insert-template '%s' not found" (expand-file-name (concat "~/.emacs.d/templates/" mode)))))))

(defun skx-insert-template-hook()
  "When loading a new file, which is empty, insert a template if we can."
  (if (= (buffer-size) 0)
      (skx-insert-template buffer-file-name)))


(add-hook 'find-file-hook 'skx-insert-template-hook)

(provide 'skx-template)
