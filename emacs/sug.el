;;; sug.el -- My personal emacs configuration.

;;; Commentary:

;; Adds alias, shortcuts and prelude preferences.

;;; Code:
;; key bindings
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "M-p") (kbd "M-5 C-p"))
(global-set-key (kbd "M-n") (kbd "M-5 C-n"))
(global-set-key (kbd "C-x p") (kbd "M-- C-x o"))
(global-set-key (kbd "C-x C-b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x r e") 'refresh-file)
(global-set-key (kbd "M-g") 'goto-line)

;; aliases
(global-set-key (kbd "C-x C-p") (kbd "C-x p"))
(global-set-key (kbd "C-x C-o") (kbd "C-x o"))
(global-set-key (kbd "C-x C-k") (kbd "C-x k"))

;; refresh file
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
)

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1))
)

;; allow arrow keys
(setq prelude-guru nil)

;; disable menu bar
(menu-bar-mode 0)

(provide 'sug)
;;; sug.el ends here
