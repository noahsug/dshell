(autoload 'css-mode "css-mode")

(setq auto-mode-alist (append
                       '(("\\.css$" . css-mode))
                       '(("\\.gss$" . css-mode))
                       '(("\\.scss$" . css-mode))
                       auto-mode-alist))

;(autoload 'sass-mode "sass-mode")
;(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

(defun on-css-loaded ()
  (when (package-installed-p 'rainbow-mode)
    (require 'rainbow-mode)
    (rainbow-turn-on)))

(add-hook 'css-mode-hook 'on-css-loaded)

(setq-default cssm-indent-level 4)
(setq-default cssm-indent-function 'cssm-c-style-indenter)
