;;; resync-packages.el - Update the packages I have installed from their source

;;
;; I store a bunch of Lisp files within this repository which originate
;; from remote URLs, mostly github repositories.
;;
;; I don't want to use submodules, nor do I want to clone repos automatically
;; instead I can use the contents of this module to resync them appropriately.
;;
;; I could not store the files themselves under revision control, but then
;; I'd be screwed if they ever disappeared..
;;


(setq resync/package-list
      '(
        ("~/.emacs.d/lang/cfengine.el"              . "https://raw.githubusercontent.com/cfengine/core/master/contrib/cfengine.el")
        ("~/.emacs.d/lang/dockerfile-mode.el"       . "https://raw.githubusercontent.com/spotify/dockerfile-mode/master/dockerfile-mode.el")
        ("~/.emacs.d/lang/go-mode.el"               . "https://raw.githubusercontent.com/dominikh/go-mode.el/master/go-mode.el")
        ("~/.emacs.d/lang/groovy-mode.el"           . "https://raw.githubusercontent.com/Groovy-Emacs-Modes/groovy-emacs-modes/master/groovy-mode.el")
        ("~/.emacs.d/lang/hcl-mode.el"              . "https://raw.githubusercontent.com/syohex/emacs-hcl-mode/master/hcl-mode.el")
        ("~/.emacs.d/lang/lua-mode.el"              . "https://raw.githubusercontent.com/immerrr/lua-mode/master/lua-mode.el")
        ("~/.emacs.d/lang/markdown-mode.el"         . "https://raw.githubusercontent.com/jrblevin/markdown-mode/master/markdown-mode.el")
        ("~/.emacs.d/lang/monkey.el"                . "https://raw.githubusercontent.com/skx/monkey/master/emacs/monkey.el")
        ("~/.emacs.d/lang/puppet-mode.el"           . "https://raw.githubusercontent.com/voxpupuli/puppet-mode/master/puppet-mode.el")
        ("~/.emacs.d/lang/terraform-mode.el"        . "https://raw.githubusercontent.com/syohex/emacs-terraform-mode/master/terraform-mode.el")
        ("~/.emacs.d/lang/web-mode.el"              . "https://raw.githubusercontent.com/fxbois/web-mode/master/web-mode.el")
        ("~/.emacs.d/lang/yaml-mode.el"             . "https://raw.githubusercontent.com/yoshiki/yaml-mode/master/yaml-mode.el")
        ("~/.emacs.d/lang/z80-mode.el"              . "https://raw.githubusercontent.com/SuperDisk/z80-mode/master/z80-mode.el")
        ("~/.emacs.d/lib/dash.el"                   . "https://raw.githubusercontent.com/magnars/dash.el/master/dash.el")
        ("~/.emacs.d/lib/s.el"                      . "https://raw.githubusercontent.com/magnars/s.el/master/s.el")
        ("~/.emacs.d/org/org-appear.el"             . "https://raw.githubusercontent.com/awth13/org-appear/master/org-appear.el")
        ("~/.emacs.d/org/org-autolist.el"           . "https://raw.githubusercontent.com/calvinwyoung/org-autolist/master/org-autolist.el")
        ("~/.emacs.d/org/org-bullets.el"            . "https://raw.githubusercontent.com/sabof/org-bullets/master/org-bullets.el")
        ("~/.emacs.d/org/org-diary.el"              . "https://raw.githubusercontent.com/skx/org-diary/master/org-diary.el")
        ("~/.emacs.d/org/org-nested-links.el"       . "https://raw.githubusercontent.com/skx/org-nested-links/master/org-nested-links.el")
        ("~/.emacs.d/org/org-tag-cloud.el"          . "https://raw.githubusercontent.com/skx/org-tag-cloud/master/org-tag-cloud.el")
        ("~/.emacs.d/tools/goto-last-change.el"     . "https://www.emacswiki.org/emacs/download/goto-last-change.el")
        ("~/.emacs.d/ui/imenu-list.el"              . "https://raw.githubusercontent.com/bmag/imenu-list/master/imenu-list.el")
        ("~/.emacs.d/ui/recentf-buffer.el"          . "https://www.emacswiki.org/emacs/download/recentf-buffer.el")
        ("~/.emacs.d/ui/smex.el"                    . "https://raw.githubusercontent.com/nonsequitur/smex/master/smex.el")
        ("~/.emacs.d/unix/exec-path-from-shell.el"  . "https://raw.githubusercontent.com/purcell/exec-path-from-shell/master/exec-path-from-shell.el")
        ))


(defun resync/packages()
  "resync/packages fetches a number of remote URLs and writes them to files beneath ~/.emacs.

  The files are fetched from the list in `resync/package-list'."
  (interactive)
  (let ((time (current-time))
        (count 0))
    (mapcar (lambda (x)
              (let* ((url (cdr x))
                     (dst (car x)))
                (message "Fetching %s" url)
                (setq count (+ count 1))
                (url-copy-file url dst t)))
            resync/package-list)
    (message "Fetched %d files in %.06f seconds" count (float-time (time-since time)))))

(defun resync/resync()
  "Resync packages from their upstream locations.

If `straight' is installed then rebuild its packages at the same time."
  (interactive)
  (resync/packages)
  (if (fboundp 'straight-pull-all)
      (progn
        (straight-pull-all)
        (straight-rebuild-all))))


(provide 'resync-packages)
