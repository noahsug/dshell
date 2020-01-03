(dolist (pattern '("\\.html"))
  (add-to-list 'auto-mode-alist (cons pattern 'html-mode)))

(defun on-html-loaded ()
  ;(require 'angular-html-mode)
  )

(add-hook 'html-mode-hook 'on-html-loaded)
