;;; sug.el -- My personal emacs configuration.

;;; Commentary:

;; Adds aliases, shortcuts and prelude preferences.

;; Code:

;; Useful emacs shortcuts
; C-M-x -> eval current expression

;;
; General key bindings
;;
(global-set-key (kbd "C-x C-g C-d") 'delete-selection-mode)
(global-set-key (kbd "C-x C-g C-s") 'subword-mode)
(global-set-key (kbd "C-x R") 'rename-file-and-buffer)
(global-set-key (kbd "C-x X") 'replace-regexp)
(global-set-key (kbd "C-c C-s") 'spell-check-on)
(global-set-key (kbd "C-c C-e") 'flyspell-mode)
(global-set-key (kbd "C-x m") 'shell)
(global-set-key (kbd "C-x e") 'eval-last-sexp)
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
;(global-set-key (kbd "C-M-z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))
;(global-set-key (kbd "C-M-Z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))
(global-set-key (kbd "C-x C-p") (kbd "M-- C-x o"))
(global-set-key (kbd "C-x C-o") (kbd "C-x o"))
(global-set-key (kbd "C-x C-k") (kbd "C-x k"))
(global-set-key (kbd "C-x C-z") (kbd "C-x z"))

;(global-set-key (kbd "C-c s a") 'save-buffers-a)
;(global-set-key (kbd "C-c s b") 'save-buffers-b)
;(global-set-key (kbd "C-c s c") 'save-buffers-c)
;
;(global-set-key (kbd "C-c r a") 'read-buffers-a)
;(global-set-key (kbd "C-c r b") 'read-buffers-b)
;(global-set-key (kbd "C-c r c") 'read-buffers-c)

;; projectile key bindings
(global-set-key (kbd "C-c j") 'projectile-test-project)
(global-set-key (kbd "C-c C-j") 'projectile-test-project)
(global-set-key (kbd "C-x C-j") 'projectile-test-project)

(global-set-key (kbd "C-c l") 'projectile-compile-project)
(global-set-key (kbd "C-c C-l") 'projectile-compile-project)

(global-set-key (kbd "C-c a") 'projectile-ack)
(global-set-key (kbd "C-c C-a") 'projectile-ack)
;(global-set-key (kbd "C-x C-a") 'projectile-ack)

(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)

(global-set-key (kbd "C-c K") 'projectile-kill-buffers)

(global-set-key (kbd "C-c R") 'projectile-replace)

(global-set-key (kbd "C-c S") 'projectile-switch-project)


;;
; Settings
;;

;; mini-buffer color
(set-face-foreground 'minibuffer-prompt "white")

;; indent level
(setq js-indent-level 2)
(setq java-indent-level 2)

;; no tabs
(setq-default indent-tabs-mode nil)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; disable version control
(setq vc-handled-backends ())

;; don't word wrap
(setq toggle-truncate-lines nil)

;; allow arrow keys
(setq prelude-guru nil)

;; follow symlinks and don't ask
(setq vc-follow-symlinks t)

;; disable menu bar
(menu-bar-mode 0)

;; highlight current line
;(set-face-background 'hl-line "#444")

;; subword mode
(add-hook 'soy-mode-hook 'subword-mode)
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'js2-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'coffee-mode-hook 'subword-mode)
(add-hook 'python-mode-hook 'subword-mode)
(eval-after-load (format "js2-emacs%d" emacs-major-version)
  '(progn
     (add-hook 'js2-mode-hook 'subword-mode)))

;; use aspell instead of ispell
;(setq ispell-list-command "--list")

;; fix for "lisp nesting exceeds `max-lisp-eval-depth'" error
(setq max-lisp-eval-depth 1000)

;; fix for "variable binding depth exceeds max-specpdl-size" error
(setq max-specpdl-size 1000)

;; show column numbers
(column-number-mode t)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;
; Functions
;;

;; refresh file
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
)

;; get face under cursor - or use C-u C-x =
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


;; save buffers to desktop (TODO)
;(desktop-save-mode 0)
(defun save-buffers-a ()
  (interactive)
  (desktop-save "~/.emacs.d/"))

;; rename file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; enable spell check - flyspell-mode again to disable
(defun spell-check-on()
  (interactive)
  (flyspell-mode)
  (flyspell-buffer)
)

;;
; Requires
;;

(add-to-list 'load-path "~/.emacs.d/personal/sug")

;; less mode
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; sass mode
(add-to-list 'auto-mode-alist '("\\.scss" . css-mode))

;; projectile
(if (not (boundp 'skip-install-projectile)) (progn
  (setq compilation-read-command nil)
  (require 'projectile)
  (projectile-global-mode)
))

;; pabbrev
(require 'pabbrev)
(global-pabbrev-mode)
(pabbrev-shut-up)

;; auto-complete
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "/usr/local/google/home/sugarman/.emacs.d/personal/sug/ac-dict")
;(ac-config-default)
;(setq ac-auto-start t)
;(define-key ac-completing-map (kbd "ESC") 'ac-stop)

;; ace-jump
(require 'ace-jump-mode)

;; key-chord
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jj" 'ace-jump-mode)

;; ido
(require 'ido)
(ido-mode t)
;(setq
;  ido-ignore-buffers ;; ignore these guys
;  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
;    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))

;; coffee-mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

;; css-mode
(add-to-list 'auto-mode-alist '("\\.gss$" . css-mode))

;; js2-mode
;(require 'js2-mode)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;(autoload 'js2-minor-mode (format "js2-emacs%d" emacs-major-version) nil t)
;(add-hook 'js-mode-hook 'js2-minor-mode)

;(defun my-add-goog-externs ()
;  (interactive)
;  (let ((buf (buffer-string))
;        (index 0))
;    (while (string-match "\\1;3201;0c(goog\\.require\\|goog\\.provide\\)('\\([^'.]*\\)" buf index)
;      (setq index (+ 1 (match-end 0)))
;      (add-to-list 'js2-global-externs (match-string 2 buf)))))
;
;(add-hook 'js2-mode-hook 'my-add-goog-externs)
;(add-hook 'js-mode-hook 'my-add-goog-externs)
;(add-to-list 'js2-global-externs "goog")
;(add-to-list 'js2-global-externs "gcomm")

;(define-key js2-mode-map (kbd "C-c C-e") 'flyspell-mode)
;(define-key js2-mode-map (kbd "C-c C-s") 'spell-check-on)

;; flymake gjlint
(require 'flymake)
(defun flymake-gjslint-init ()
  "Initialize flymake for gjslint"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace)))
    ;; (list "gjslint" (list temp-file "--nosummary"))))
    (list "gjslint" (list temp-file "--nosummary"))))
(add-to-list 'flymake-allowed-file-name-masks
             '(".+\\.js$"
               flymake-gjslint-init
               flymake-simple-cleanup
               flymake-get-real-file-name))
(add-to-list 'flymake-err-line-patterns
             '("^Line \\([[:digit:]]+\\), E:[[:digit:]]+: "
               nil 1 nil))


;; whitespace mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; multi web mode (HTML + JS)
(require 'multi-web-mode)
(setq mweb-default-major-mode 'js-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;                  (js2-mode "<script.*>" "</script>")
                  (js-mode "<script.*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; jump-char
(require 'jump-char)
(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(control meta m)] 'jump-char-backward)

;; revbufs
(require 'revbufs)
(global-set-key (kbd "C-x r a") 'revbufs)

;; switch to corresponding test/impl file
(require 'swap-impl-test)
(global-set-key (kbd "C-c t") 'swap-impl-test)
(global-set-key (kbd "C-c C-t") 'swap-impl-test)

;; expands a phrase to fit into a template
(require 'template-expand)
(global-set-key (kbd "M-j") 'template-expand)

;; soy-mode
;(require 'soy-mode)

;; better soy-mode
(require 'closure-template-html-mode)
(add-to-list 'auto-mode-alist '("\\.soy$" . closure-template-html-mode))

;; color for compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; colorize color codes
;(require 'rainbow-mode "~/.emacs.d/personal/sug/rainbow-mode.el")

;; flycheck
;(require 'flycheck "~/.emacs.d/personal/sug/flycheck.el")
;(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'sug)
;;; sug.el ends here
