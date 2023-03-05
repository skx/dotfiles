;;; my-todo.el -- Highlight words relating to bugs/etc.

;; This is a simple replacement for the core of `hl-todo.el`.

(defvar my/todo-keywords
  '(("TODO"  . "#ffff00")
    ("FIXME" . "#ffff00")
    ("XXX+"  . "#ffff00")))

(defun my/todo-fontify ()
  (unless (derived-mode-p 'org-mode)
    (font-lock-add-keywords
     nil
     (mapcar (lambda (x)
               `(,(concat "\\<" (car x) "\\>") 0 '(:weight bold :foreground ,(cdr x)) prepend))
             my/todo-keywords))))

(add-hook 'prog-mode-hook #'my/todo-fontify)
(add-hook 'text-mode-hook #'my/todo-fontify)

(provide 'my-todo)
