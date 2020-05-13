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
;;



(defvar skx-template-prefix "~/.emacs.d/templates/"
  "The location from which templates are loaded ")

(defun skx-get-file-lang-mode (file)
  "Given a filename return the language-mode which will be used to handle it,
   via inspection of auto-mode-alist.

   Returns an empty string if the mode could not be determined."
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
      nil)))

(defun skx-insert-template(name)
  "Given the name of a buffer we attempt to insert the appropriate template.

   Templates are discovered beneath the skx-template-prefix directory, and
   are named after the mode of the buffer.

   Environmental variables will be expanded within the template, and the
   point will be moved to the position of '@@' if it is present within the
   contents of the expanded buffer."
  (let* ((mode (skx-get-file-lang-mode name))
         (src nil))

    ;; If we found a mode then set the source-filename to the appropriate value
    (if mode
        (setq src (expand-file-name (concat skx-template-prefix mode))))

    ;; If we have a source, and the source exists, then insert it.
    (if (and src
             (file-exists-p src))
        (progn

          ;; insert template-contents.
          (insert-file-contents src)

          ;; expand ${USER} -> "skx"
          (skx-template-expand-buffer-contents)

          ;; if @@ is present we'll move the cursor there,
          ;; otherwise the end of the buffer.
          (if (re-search-forward "@@" nil t)
              (delete-backward-char 2)
            (end-of-buffer)
          )))

    ;; If we have a mode, but there is no source template, then show that
    (if (and mode
             (not (file-exists-p src)))
        (message "mode '%s' template missing '%s'" mode src))))

(defun skx-template-expand-buffer-contents ()
  "Replace all environmental variables, expressed as ${FOO}, with
  their actual contents.

   If an environmental variable is empty/unset then the replacement
  is left alone."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "$\{\\([A-Z]+\\)\}" nil t)
      (replace-match
       (format "%s" (or (getenv (match-string 1))
                        (concat "${" (match-string 1) "}" ))) t))
    ))


(defun skx-insert-template-hook()
  "When loading a new file, which is empty, insert a template if we can."
  (if (= (buffer-size) 0)
      (skx-insert-template buffer-file-name)))


(add-hook 'find-file-hook 'skx-insert-template-hook)

(provide 'skx-template)
