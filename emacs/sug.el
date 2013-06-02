;;; sug.el -- My personal emacs configuration.

;;; Commentary:

;; Adds aliases, shortcuts and prelude preferences.

;;; Code:
;; key bindings
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "M-p") (kbd "M-5 C-p"))
(global-set-key (kbd "M-n") (kbd "M-5 C-n"))
(global-set-key (kbd "C-x p") (kbd "M-- C-x o"))
(global-set-key (kbd "C-x C-b") (kbd "C-x b C-m"))
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x r e") 'refresh-file)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-j") 'hippie-expand)
(global-set-key (kbd "C-M-j") 'pabbrev-scavenge-buffer-fast)

(global-set-key (kbd "C-c j") 'projectile-test-project)
(global-set-key (kbd "C-c C-j") 'projectile-test-project)
(global-set-key (kbd "C-x C-j") 'projectile-test-project)

(global-set-key (kbd "C-c l") 'projectile-compile-project)
(global-set-key (kbd "C-c C-l") 'projectile-compile-project)

(global-set-key (kbd "C-c a") 'projectile-ack)
(global-set-key (kbd "C-c C-a") 'projectile-ack)
(global-set-key (kbd "C-x C-a") 'projectile-ack)

(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)

(global-set-key (kbd "C-c K") 'projectile-kill-buffers)
(global-set-key (kbd "C-c C-K") 'projectile-kill-buffers)

(global-set-key (kbd "C-c R") 'projectile-replace)
(global-set-key (kbd "C-c C-R") 'projectile-replace)

(global-set-key (kbd "C-c S") 'projectile-switch-project)
(global-set-key (kbd "C-c C-S") 'projectile-switch-project)

(global-set-key (kbd "C-c t") 'projectile-toggle-between-implemenation-and-test)
(global-set-key (kbd "C-c C-t") 'projectile-toggle-between-implemenation-and-test)

(global-set-key (kbd "C-M-z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))

(global-set-key (kbd "C-M-Z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

;(global-set-key (kbd "C-c c") (kbd "C-c p p"))
;(global-set-key (kbd "C-c l") (kbd "C-c p l"))
;(global-set-key (kbd "C-c a") (kbd "C-c p a"))
;(global-set-key (kbd "C-c f") (kbd "C-c p f"))
;(global-set-key (kbd "C-c K") (kbd "C-c p k"))
;(global-set-key (kbd "C-c R") (kbd "C-c p r"))
;(global-set-key (kbd "C-c S") (kbd "C-c p s"))
;(global-set-key (kbd "C-c t") (kbd "C-c p t"))

;; aliases
(global-set-key (kbd "C-x C-p") (kbd "C-x p"))
(global-set-key (kbd "C-x C-o") (kbd "C-x o"))
(global-set-key (kbd "C-x C-k") (kbd "C-x k"))
(global-set-key (kbd "C-x C-z") (kbd "C-x z"))

;; refresh file
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
)

;; allow arrow keys
(setq prelude-guru nil)

;; disable menu bar
(menu-bar-mode 0)

;; jump-char
(require 'jump-char "~/.emacs.d/personal/sug/jump-char/jump-char.el")
(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(control meta m)] 'jump-char-backward)

;; revbufs
(require 'revbufs "~/.emacs.d/personal/sug/jump-char/revbufs.el")
(global-set-key (kbd "C-x r a") 'revbufs)

;; fix for "lisp nesting exceeds `max-lisp-eval-depth'" error
(setq max-lisp-eval-depth 1000)

;; fix for "variable binding depth exceeds max-specpdl-size" error
(setq max-specpdl-size 1000)

;; ido
(setq
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))

;; color for compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(test-suffix)

(provide 'sug)
;;; sug.el ends here
