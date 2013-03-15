;;  We always prefer CPerl mode to Perl mode.
(fset 'perl-mode 'cperl-mode)
(setq auto-mode-alist
      (append '(("\\.t$" . cperl-mode)) auto-mode-alist))

;;  When starting load my hooks
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook t)

;;  BSD Style brace placement, with tab=4 spaces.
(defun my-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-brace-offset -2)
  (setq cperl-label-offset 0))



(provide 'skx-perl)