;;; pwsafe.el --- emacs interface to pwsafe

;; Copyright (C) 2006-2007 by Stefan Reichoer

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; pwsafe.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; pwsafe.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; pwsafe.el provides an Emacs interface for pwsafe:
;; http://nsd.dyndns.org/pwsafe, http://passwordsafe.sourceforge.net

;; The latest version of pwsafe.el can be found at:
;; http://www.xsteve.at/prg/emacs/

;; Usage:
;; put the following in your .emacs:
;; (require 'pwsafe)
;; Then run M-x pwsafe
;; - if the password database file does not yet exist it will be created now
;; - otherwise a list of nicknames for your stored passwords is shown

;; It is also possible to feed passwords from the pwsafe database
;; when emacs asks for a password. This is done by advising the read-password function
;; See the documentation for the variable pwsafe-feed-read-passwd-rules for details

;; I am very interested in feedback about the usability and about
;; security concerns for this package

;; Before using pwsafe.el, I used a simple unencrypted text file
;; for my passwords. So this package enhances the security for my use case.

;;; History:
;;

;;; Code:

(defvar pwsafe-primary-database "~/.pwsafe.dat" "The primary database used for pwsafe")
;;(defvaralias 'pwsafe-database 'pwsafe-primary-database) ;; compatibility alias

(defvar pwsafe-secondary-databases nil "A list of file names for additional pwsafe databases") ;; not yet used...

(defvar pwsafe-use-ido-completion
  (fboundp 'ido-completing-read) "Whether to use ido completion")

(defvar pwsafe-keep-passwd nil "Whether to keep the passwd cached

When nil, don't cache the passwd
When a number: cache the passwd for the given number of seconds
Otherwise cache the passwd for ever")

(defvar pwsafe-use-long-listing nil "Display comments for the pwsafe entries.
This means that pwsafe -l is used to get the database entries.")

(defvar pwsafe-feed-read-passwd-rules nil
  "An alist that allows to feed read-passwd calls with passwords from pwsafe.

 Some examples for pwsafe-feed-read-passwd-rules
 ((\"caller1\" . \"bmk-name\")    ;; Use bmk-name
  (\"caller2\" . nil)             ;; call read-passwd, don't use pwsafe
  (\"caller3\" . some-function)   ;; call function with two parameters
                                  ;; (the caller and the prompt, for an example see:
                                  ;; `pwsafe-read-passwd-select-bookmark-callback'
  (t . some-function))            ;; matches always and calls the some-function

Use M-x `pwsafe-identify-next-read-passwd-caller' to find out the needed caller functions above.
")

(defvar pwsafe-verbose-read-passwd-advice t
  "Whether the read-passwd advice should display the caller in the *Messages* buffer")

;; internal variables
(defvar pwsafe-password-prompt-regexp
  "[Pp]ass\\(word\\|phrase\\).*:\\s *\\'"
  "*Regexp matching prompts for passwords for pwsafe.")

(defvar pwsafe-number-of-passwd-retrys nil)

(defvar pwsafe-keep-passwd-timer nil)
(defvar pwsafe-cached-passwd nil)

;; taken from DVC.el
(defsubst pwsafe-face-add (str face &optional keymap menu help)
  "Add to string STR the face FACE.
Optionally, also add the text properties KEYMAP, MENU and HELP.

If KEYMAP is a symbol, (symbol-value KEYMAP) is used
as a keymap; and `substitute-command-keys' result
against (format \"\\{%s}\" (symbol-name keymap)) is appended to HELP.

If HELP is nil and if MENU is non nil, the MENU title is used as HELP."
  (let* ((strcpy (copy-sequence str))
         (key-help (when (symbolp keymap)
                     (substitute-command-keys (format "\\{%s}" (symbol-name keymap)))))
         (prefix-help (if help help (when (and menu (stringp (cadr menu))) (cadr menu))))
         (long-help (if key-help
                        (if prefix-help (concat prefix-help "\n"
                                                "================" "\n"
                                                key-help) key-help)
                      help))
         (keymap (if (symbolp keymap) (symbol-value keymap) keymap)))
    (add-text-properties 0 (length strcpy)
                         `(face ,face
                                font-lock-face ,face
                                ,@(when keymap
                                    `(mouse-face highlight
                                                 keymap ,keymap
                                                 help-echo ,long-help))
                                ,@(when menu
                                    `(,pwsafe-cmenu ,menu))
                                )
                         strcpy)
    strcpy))

(defun pwsafe-clear-passwd-cache ()
  "Clear the cached passwd in `pwsafe-cached-passwd'."
  (interactive)
  (when (interactive-p)
    (message (if pwsafe-cached-passwd "Cleared the cached pwsafe password" "pwsafe password not cached")))
  (setq pwsafe-cached-passwd nil))

(defun pwsafe-read-passwd (prompt)
  (when pwsafe-cached-passwd
    (message "pwsafe: Using cached passwd"))
  (let ((passwd (or pwsafe-cached-passwd (read-passwd prompt))))
    (when pwsafe-keep-passwd
      (setq pwsafe-cached-passwd passwd)
      (when (numberp pwsafe-keep-passwd)
        (when (timerp pwsafe-keep-passwd-timer)
          (cancel-timer pwsafe-keep-passwd-timer))
        (setq pwsafe-keep-passwd-timer (run-with-timer pwsafe-keep-passwd nil 'pwsafe-clear-passwd-cache))))
    passwd))

(defun pwsafe-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (if (string-match pwsafe-password-prompt-regexp string)
        (progn (string-match "^\\([^\n]+\\)\n*\\'" string)
               ;;(message "pwd prompt: '%s'" string)
               (setq pwsafe-number-of-passwd-retrys (+ pwsafe-number-of-passwd-retrys 1))
               (when (> pwsafe-number-of-passwd-retrys 1)
                 (pwsafe-clear-passwd-cache))
               (if (<= pwsafe-number-of-passwd-retrys 3)
                   (let ((passwd (pwsafe-read-passwd (match-string 1 string))))
                     (process-send-string proc (concat passwd "\n")))
                 (kill-process proc)
                 (sit-for 0.3)
                 (message "Entered a wrong password 3 times - pwsafe aborted.")))
      (insert string)
      (cond ((member pwsafe-running-command '(copy-passwd copy-user-and-passwd))
             (dolist (line (split-string string "\n"))
               (when (string-match "\\(You are ready to paste\\|Sending \\(username\\|password\\) for\\)" line)
                 (message (format "pwsafe: %s" line)))))
            ((eq pwsafe-running-command 'add)
                                        ;(message "  pwsafe-process-filter: %s" string)
             (dolist (line (split-string string "\n"))
               (when (string-match "^\\(group \\[<none>\\]\\|username\\|notes\\): " line)
                 (let ((answer (read-string (concat "pwsafe add " line))))
                   (process-send-string proc (concat answer "\n"))))
               (when (string-match "Generate random password\\? \\[y\\]" line)
                 (process-send-string proc "y"))
               (when (string-match "^type .+ length .+ bits of entropy" line)
                 (let ((answer (read-char-exclusive (concat "pwsafe password: " line))))
                   (process-send-string proc (char-to-string answer))))))
            ((eq pwsafe-running-command 'edit)
             ;;(message "  pwsafe-process-filter 'edit: %s" string)
             (dolist (line (split-string string "\n"))
               ;; name: [hurz]
               (when (string-match "^\\(name\\|group\\|username\\): \\[\\(.*\\)\\]" line)
                 (let ((answer (read-string line (match-string 2 line))))
                   (process-send-string proc (concat answer "\n"))))
               (when (string-match "^change password" line)
                 (process-send-string proc (if (y-or-n-p "Change password ") "y" "n")))
               (when (string-match "new password: .return for random." line)
                 (process-send-string proc (concat (read-passwd (concat line ": ")) "\n")))
               (when (string-match "^notes: " line)
                 (process-send-string proc
                                      (let ((notes (read-string "Notes: "
                                                                (with-current-buffer "*pwsafe-list*" (nth 2 (pwsafe-list-line-info))))))
                                        (concat notes "\n"))))
               (when (string-match "^Confirm changing .+ " line)
                 (process-send-string proc "y")))
             )))))

(defun pwsafe-run (cmd &rest args)
  (with-current-buffer (get-buffer-create "*pwsafe*")
    (delete-region (point-min) (point-max)))
  (let ((process (apply 'start-process "pwsafe" "*pwsafe*" "pwsafe" args)))
    (setq pwsafe-running-command cmd)
    (setq pwsafe-number-of-passwd-retrys 0)
    (set-process-filter process 'pwsafe-process-filter)
    (set-process-sentinel process 'pwsafe-process-sentinel)))

(defun pwsafe-process-sentinel (process event)
  (save-excursion
    (set-buffer (process-buffer process))
    (cond ((string= event "finished\n")
           (cond ((eq pwsafe-running-command 'createdb)
                  (message "Created pwsafe database %s" (pwsafe-current-database-name)))
                 ((eq pwsafe-running-command 'list)
                  (pwsafe-list-passwords))
                 ((eq pwsafe-running-command 'copy-passwd)
                  );; do nothing here
                 ((eq pwsafe-running-command 'copy-user-and-passwd)
                  );; do nothing here
                 ((eq pwsafe-running-command 'delete)
                  (message "pwsafe delete finished"))
                 ((eq pwsafe-running-command 'extract)
                  );; do nothing here
                 (t
                  (message "pwsafe process finished")))
           (setq pwsafe-running-command nil))
          ((string= event "killed\n")
           (message "pwsafe process killed")
           (setq pwsafe-running-command nil))
          ((string= event "terminated\n")
           (message "pwsafe process terminated")
           (setq pwsafe-running-command nil))
          ((string-match "exited abnormally" event)
           (while (accept-process-output process 0 100))
           ;; find last error message and show it.
           (goto-char (point-max))
           (message "pwsafe failed: %s" event)
           (setq pwsafe-running-command nil))
          (t
           (message "pwsafe process had unknown event: %s" event)))))

(defun pwsafe-current-database-name ()
  (if (eq major-mode 'pwsafe-list-mode)
      (save-excursion
        (end-of-line)
        (if (re-search-backward "Database \<\\(.+\\)\>" nil t)
            (match-string-no-properties 1)
          (expand-file-name pwsafe-primary-database)))
    (expand-file-name pwsafe-primary-database)))

(defun pwsafe-completing-read (prompt choices &optional predicate require-match initial-input hist def)
  (funcall (if pwsafe-use-ido-completion 'ido-completing-read 'completing-read)
           prompt choices predicate require-match initial-input hist def))

(defun pwsafe-createdb ()
  "Run pwsafe --createdb"
  (interactive)
  (let ((database-file-name (pwsafe-current-database-name)))
    (if (file-exists-p database-file-name)
        (message "The pwsafe database %s does already exist." database-file-name)
      (pwsafe-run 'createdb "--createdb" "-f" database-file-name)
      (message "Created pwsafe database %s" database-file-name))))

;;;###autoload
(defun pwsafe (force)
  "Major mode to interact with the command line password safe pwsafe.
Queries the passwords from the password safe and displays them in the buffer *pwsafe-list*.
The following keys are defined:
\\{pwsafe-list-mode-map}"
  (interactive "P")
  (if (and (not force)
           (not (string= (buffer-name) "*pwsafe-list*"))
           (get-buffer "*pwsafe-list*"))
      (pop-to-buffer "*pwsafe-list*")
    (let ((database-file-name (pwsafe-current-database-name)))
      (if (file-exists-p database-file-name)
          (if pwsafe-use-long-listing
              (pwsafe-run 'list "-l" "-f" database-file-name)
            (pwsafe-run 'list "-f" database-file-name))
        (when (yes-or-no-p (format "pwsafe database %s does not exist - create it? " database-file-name))
          (pwsafe-createdb)
          (with-current-buffer (get-buffer "*pwsafe*")
            (delete-region (point-min) (point-max)))
          (pwsafe-list-passwords))))))

(defun pwsafe-insert-password-entry (entry)
  (let ((start-pos (point))
        (prefix-string ""))
    (when pwsafe-secondary-databases
      (setq prefix-string "  "))
    (insert (pwsafe-face-add (format "%s%s" prefix-string (car entry)) 'font-lock-function-name-face))
    (when (cadr entry)
      (insert (format " - %s" (cadr entry))))
    (newline)
    (when (nth 2 entry)
      (insert (format "%s  %s\n" prefix-string (nth 2 entry))))
    (setq overlay (make-overlay start-pos (point)))
    (overlay-put overlay 'pwsafe entry)))

(defun pwsafe-list-passwords ()
  (interactive)
  (let ((pwlist)
        (database-name (pwsafe-current-database-name))
        (current-pw)
        (overlay))
    (with-current-buffer (get-buffer "*pwsafe*")
      (goto-char (- (point-max) 1))
      (re-search-backward "^$")
      (forward-line 1)
      (while (< (point) (- (point-max) 2))
        (if (looking-at "^> \\(.+\\)") ; notes line from previous entry
            (setcar pwlist (append (car pwlist) (list (match-string 1))))
          (if (looking-at "^\\(.+\\)  -  \\(.+\\)")
              (add-to-list 'pwlist (list (match-string 1) (match-string 2)))
            (add-to-list 'pwlist (list (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil))))
        (forward-line 1)))
    (pop-to-buffer "*pwsafe-list*")
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (when pwsafe-secondary-databases
        (insert (pwsafe-face-add (format "Database <%s>\n" database-name) 'font-lock-variable-name-face)))
      (unless pwlist
        (insert "Empty pwsafe database."))
      ;; e contains: bmk-full-name, user-name, comment
      (dolist (e (nreverse pwlist))
        (pwsafe-insert-password-entry e)))
    (goto-char (point-min))
    (pwsafe-list-mode)))

(defvar pwsafe-list-mode-map () "Keymap used in `pwsafe-list-mode' buffers.")
(defvar pwsafe-list-select-map () "Subkeymap used in `pwsafe-list-mode' for select and operate commands.")

(when (not pwsafe-list-mode-map)
  (setq pwsafe-list-mode-map (make-sparse-keymap))
  (define-key pwsafe-list-mode-map [?q] 'bury-buffer)
  (define-key pwsafe-list-mode-map [?n] 'pwsafe-next)
  (define-key pwsafe-list-mode-map [?p] 'pwsafe-previous)
;  (define-key pwsafe-list-mode-map [?j] 'pwsafe-jump)
  (define-key pwsafe-list-mode-map [?g] 'pwsafe)
  (define-key pwsafe-list-mode-map [?x] 'pwsafe-clear-passwd-cache)
  (define-key pwsafe-list-mode-map [?l] 'pwsafe-toggle-use-long-listing)
  (define-key pwsafe-list-mode-map [?a] 'pwsafe-add-entry)
  (define-key pwsafe-list-mode-map [?e] 'pwsafe-edit-entry)
  (define-key pwsafe-list-mode-map [(control ?d)] 'pwsafe-delete-entry)
  (define-key pwsafe-list-mode-map [?U] 'pwsafe-copy-user-name)
  (define-key pwsafe-list-mode-map [?P] 'pwsafe-copy-password)
  (define-key pwsafe-list-mode-map [?B] 'pwsafe-copy-user-name-and-password)
  (define-key pwsafe-list-mode-map [?I] 'pwsafe-info-current-item))

(when (not pwsafe-list-select-map)
  (setq pwsafe-list-select-map (make-sparse-keymap))
  (define-key pwsafe-list-select-map (kbd "u") 'pwsafe-select-and-copy-user-name)
  (define-key pwsafe-list-select-map (kbd "p") 'pwsafe-select-and-copy-password)
  (define-key pwsafe-list-select-map (kbd "b") 'pwsafe-select-and-copy-user-name-and-password)
  (define-key pwsafe-list-mode-map (kbd "s") pwsafe-list-select-map))

(easy-menu-define pwsafe-mode-menu pwsafe-list-mode-map
"`pwsafe-list-mode' menu"
                  '("PWsafe"
                    ["Copy user name, then password" pwsafe-copy-user-name-and-password t]
                    ["Copy user name" pwsafe-copy-user-name t]
                    ["Copy password" pwsafe-copy-password t]
                    ["Add new entry" pwsafe-add-entry t]
                    ["Edit entry" pwsafe-edit-entry t]
                    ["Delete entry" pwsafe-delete-entry t]
                    ["Show comment" pwsafe-info-current-item t]
                    "--"
                    ["Jump to a bookmark" pwsafe-jump t]
                    ["Clear cached passwd" pwsafe-clear-passwd-cache t]
                    ))

(defun pwsafe-list-mode ()
  "Major Mode to entries from pwsafe."
  (interactive)
  (kill-all-local-variables)
  (toggle-read-only 1)
  (when (fboundp 'hl-line-mode)
    (hl-line-mode 1))
  (use-local-map pwsafe-list-mode-map)
  (setq major-mode 'pwsafe-list-mode)
  (setq mode-name "pwsafe-list"))

(defun pwsafe-overlay-at-point ()
  (let ((overlay-list nil))
    (dolist (overlay (overlays-at (point)))
      (when (overlay-get overlay 'pwsafe)
        (add-to-list 'overlay-list overlay)))
    (car (delete nil overlay-list))))

(defun pwsafe-list-line-info ()
  (let ((overlay (pwsafe-overlay-at-point)))
    (when overlay
      (overlay-get overlay 'pwsafe))))

(defun pwsafe-next ()
  (interactive)
  (let ((next (next-overlay-change (point))))
    (when (< next (point-max))
      (goto-char next)
      (beginning-of-line-text)
      t)))

(defun pwsafe-previous ()
  (interactive)
  (beginning-of-line)
  (goto-char (previous-overlay-change (point)))
  (beginning-of-line-text))

(defun pwsafe-copy-user-name ()
  (interactive)
  (let ((info (pwsafe-list-line-info)))
    (pwsafe-run 'copy-passwd "-u" "-f" (pwsafe-current-database-name) (car info))))

(defun pwsafe-copy-password ()
  (interactive)
  (let ((info (pwsafe-list-line-info)))
    (pwsafe-run 'copy-passwd "-p" "-f" (pwsafe-current-database-name) (car info))))

(defun pwsafe-copy-user-name-and-password ()
  (interactive)
  (let ((info (pwsafe-list-line-info)))
    (pwsafe-run 'copy-user-and-passwd "-u" "-p" "-f" (pwsafe-current-database-name) (car info))))

;;;###autoload
(defun pwsafe-add-entry (name)
  (interactive "spwsafe add bookmark name: ")
  (pwsafe-run 'add "-f" (pwsafe-current-database-name) "--add" name))

(defun pwsafe-edit-entry ()
  (interactive)
  (let ((info (pwsafe-list-line-info)))
    (pwsafe-run 'edit "-f" (pwsafe-current-database-name) "--edit" (car info))))

(defun pwsafe-delete-entry ()
  (interactive)
  (let ((info (pwsafe-list-line-info)))
    (when (yes-or-no-p (format "Delete pwsafe entry for %s? " (car info)))
      (pwsafe-run 'delete "-f" (pwsafe-current-database-name) "--delete" (car info)))))

(defun pwsafe-toggle-use-long-listing ()
  "Toggle listing of the comments for the pwsafe database entries."
  (interactive)
  (setq pwsafe-use-long-listing (not pwsafe-use-long-listing))
  (pwsafe t))

(defun pwsafe-select-bookmark-name (prompt)
  (save-excursion
    (unless (get-buffer "*pwsafe-list*")
      (pwsafe nil))
    (with-current-buffer "*pwsafe-list*"
      (let ((continue t)
            (names))
        (save-excursion
          (goto-char (point-min))
          (while continue
            (add-to-list 'names (car (pwsafe-list-line-info)))
            (setq continue (pwsafe-next)))
          (pwsafe-completing-read prompt (delete nil (nreverse names))))))))

(defun pwsafe-jump()
  "Jump to a bookmark location."
  (interactive)
  (let ((continue t)
        (dest (pwsafe-select-bookmark-name "Select: ")))
    (goto-char (point-min))
    (while continue
      (setq continue (not (string= (car (pwsafe-list-line-info)) dest)))
      (when continue
        (pwsafe-next)))))

(defun pwsafe-select-and-copy-password ()
  (interactive)
  (pwsafe-jump)
  (pwsafe-copy-password))

(defun pwsafe-select-and-copy-user-name ()
  (interactive)
  (pwsafe-jump)
  (pwsafe-copy-user-name))

(defun pwsafe-select-and-copy-user-name-and-password ()
  (interactive)
  (pwsafe-jump)
  (pwsafe-copy-user-name-and-password))

(defun pwsafe-bmk-name-split (bmk-full-name)
  "Splits a full pwsafe bookmark name in its group and name part."
  (let ((parts (split-string bmk-full-name "\\."))
        (bmk-group-name)
        (bmk-name))
    (cond ((= (length parts) 1)
           (setq bmk-name (car parts)))
          ((= (length parts) 2)
           (setq bmk-group-name (car parts))
           (setq bmk-name (cadr parts)))
          (t
           (setq bmk-group-name (car parts))
           (setq bmk-name (mapconcat 'identity (cdr parts) "."))))
    (list bmk-group-name bmk-name)))

;; bmk-group bmk-name login passwd notes
(defun pwsafe-info-list (bmk-full-name &optional get-user-name get-passwd get-notes)
  (save-excursion
    (let ((args '(extract))
          (bmk-group-name)
          (bmk-name))
      (when get-user-name
        (add-to-list 'args "-u" t))
      (when get-passwd
        (add-to-list 'args "-p" t))
      (when get-notes
        (add-to-list 'args "-l" t))
      (setq args (append args (list "-E" "-f" (pwsafe-current-database-name) bmk-full-name)))
      (apply 'pwsafe-run args)
      (while pwsafe-running-command
        (sit-for 0.1))
      (setq bmk-group-name (car (pwsafe-bmk-name-split bmk-full-name)))
      (setq bmk-name (cadr (pwsafe-bmk-name-split bmk-full-name)))
      (let ((username)
            (passwd)
            (notes))
        (with-current-buffer "*pwsafe*"
          (goto-char (point-min))
          (when (re-search-forward "^username for .+: " nil t)
            (setq username (buffer-substring (point) (line-end-position))))
          (when (re-search-forward "^password for .+: " nil t)
            (setq passwd (buffer-substring (point) (line-end-position))))
          (when (re-search-forward "^> " nil t)
            (setq notes (buffer-substring (point) (line-end-position)))))
        (kill-buffer "*pwsafe*")
        ;;(message "bmk-group: %s bmk-name: %s username: %s passwd: %s notes: %s" bmk-group-name bmk-name username passwd notes)
        (list bmk-group-name bmk-name username passwd notes)))))
;;pwsafe -E -u -p -l my-entry
;;Going to print login and password to stdout
;;WARNING: pwsafe unable to use secure ram (need to be setuid root)
;;Enter passphrase for /home/srei/.pwsafe.dat:
;;username for my-entry: user
;;password for my-entry: my-secret-pwd
;;> my funny comments

(defun pwsafe-info-current-item ()
  (interactive)
  (let* ((comment (nth 4 (pwsafe-info-list (car (pwsafe-list-line-info)) nil nil t)))
         (line-info (pwsafe-list-line-info))
         (overlay (pwsafe-overlay-at-point))
         (o-start (overlay-start overlay))
         (o-end (overlay-end overlay)))
    (message (format "Comment: %s" (or comment "<None>")))
    (if (< (length line-info) 3)
        (add-to-list 'line-info comment t)
      (setcar (nthcdr 2 line-info) comment))
    (let ((buffer-read-only nil))
      (delete-overlay overlay)
      (delete-region o-start (- o-end 1))
      (pwsafe-insert-password-entry line-info)
      (delete-char 1)
      (goto-char o-start))))

;;(defun pwsafe-passwd-current-item ()
;;  (interactive)
;;  (let ((passwd (nth 3 (pwsafe-info-list (car (pwsafe-list-line-info)) nil t nil))))
;;    (message (format "Passwd: %s" passwd))))


(defun pwsafe-read-passwd-select-bookmark-callback (caller passwd-prompt)
  (pwsafe-select-bookmark-name passwd-prompt))

;; evaluate the following code and do M-: (pwsafe-test-caller)
;;(defun pwsafe-test-caller ()
;;  (interactive)
;;  (read-passwd "test passwd: "))
;;
;;(setq pwsafe-feed-read-passwd-rules
;;  '(("huhu" . "fred") ("pwsafe-test-caller" . pwsafe-read-passwd-select-bookmark-callback)))

(defvar pwsafe-identify-next-read-passwd-caller nil)
(defun pwsafe-identify-next-read-passwd-caller ()
  "Advices `read-passwd' in a way that displays the caller of read-passwd.
This helps to create the `pwsafe-feed-read-passwd-rules'."
  (interactive)
  (setq pwsafe-identify-next-read-passwd-caller t)
  (pwsafe-activate-read-passwd-advice)
  (message "Please perform the action that will ask for a password, the caller will be copied to the kill-ring."))

(defun pwsafe-identify-read-passwd-caller ()
  (let ((caller)
        (caller-prompt))
    (with-temp-buffer
      (with-output-to-temp-buffer (buffer-name (current-buffer)) (backtrace))
      (save-match-data
        (goto-char (point-min))
        (re-search-forward "read-passwd(\"\\(.+\\)\")")
        (setq caller-prompt (match-string 1))
        (forward-line 1)
        (while (looking-at "  (")
          (forward-line 1))
        (beginning-of-line)
        (looking-at "  \\(.+\\)(")
        (setq caller (match-string 1))
        (when (or pwsafe-verbose-read-passwd-advice pwsafe-identify-next-read-passwd-caller)
          (message "pwsafe: caller for read-passwd is: %s, prompt: '%s'" caller caller-prompt))))
    (list caller caller-prompt)))

(defun pwsafe-activate-read-passwd-advice ()
  (interactive)
  (when (or pwsafe-feed-read-passwd-rules pwsafe-identify-next-read-passwd-caller)
    (defadvice read-passwd
      (around pwsafe-feed-read-passwd activate compile)
      "Allow to feed read-passwd with data from pwsafe.
See `pwsafe-feed-read-passwd-rules' for details."
      (let ((caller)
            (caller-prompt)
            (r)
            (rule)
            (action))
        (setq r (pwsafe-identify-read-passwd-caller))
        (setq caller (car r))
        (setq caller-prompt (cadr r))
        (if pwsafe-identify-next-read-passwd-caller
            (progn
              (setq pwsafe-identify-next-read-passwd-caller nil)
              (kill-new caller))
          (setq rule (assoc caller pwsafe-feed-read-passwd-rules))
          (unless rule
            (setq rule (assoc t pwsafe-feed-read-passwd-rules)))
          (when rule
            (setq action (cdr rule))
            ;;(message "rule match: %S, action: %s" rule action)
            (when (functionp action)
              (setq action (apply action (list caller caller-prompt))))
            (cond ((stringp action)
                   ;;(message "should get pwsafe password for '%s'" action)
                   (setq ad-return-value (nth 3 (pwsafe-info-list action nil t nil))))))
          ;;((eq action nil)
          ;; ad-do-it)))
          (unless ad-return-value
            ad-do-it))))))

;;(ad-remove-advice 'read-passwd 'around 'pwsafe-feed-read-passwd)
;;(ad-deactivate 'read-passwd)

(pwsafe-activate-read-passwd-advice)

(provide 'pwsafe)

;;; arch-tag: 900b1054-5492-4b81-ae06-54a5d48e6aca
;;; pwsafe.el ends here


