(defun on-cc-loaded ()
  (subword-mode))

(require 'auto-complete-c-headers)
(add-to-list 'ac-sources 'ac-source-c-headers)

(add-hook 'cc-mode-hook 'on-cc-loaded)
