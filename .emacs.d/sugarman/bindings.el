(global-set-key "\M-p" (kbd "M-5 C-p"))
(global-set-key "\M-n" (kbd "M-5 C-n"))

(global-set-key "\C-xp" (kbd "M-- C-x o"))
(global-set-key "\C-x\C-b" (kbd "C-x b C-m"))
(global-set-key "\C-x\C-p" (kbd "M-- C-x o"))
(global-set-key "\C-x\C-o" (kbd "C-x o"))
(global-set-key "\C-x\C-k" (kbd "C-x k"))
(global-set-key "\C-x\C-z" (kbd "C-x z"))

(global-set-key "\C-xb" 'switch-to-buffer)
(global-set-key "\C-xB" 'ibuffer)
(global-set-key "\C-xre" 'refresh-file)

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xX" 'replace-regexp)

(global-set-key "\C-xE" 'eval-last-sexp)

(global-set-key "\C-x\C-g\C-d" 'delete-selection-mode)
(global-set-key "\C-x\C-g\C-s" 'subword-mode)

(global-set-key "\C-xR" 'rename-file-and-buffer)
(global-set-key "\C-xF" 'ack-and-a-half)

(global-set-key "\C-c\C-s" 'spell-check-on)
(global-set-key "\C-c\C-e" 'flyspell-mode)

(global-set-key "\M-?" 'help-command)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [f5] 'revert-buffer)

(global-set-key "\C-xc" 'compile)

(global-set-key "\C-xm" 'eshell)
(global-set-key "\C-x\S-s" 'eshell)

(global-set-key "\C-x\C-w" 'delete-blank-lines-and-trailing-whitespace)
(global-set-key "\C-xW" 'write-file)

(global-set-key [(control ?/)] 'PC-lisp-complete-symbol)

(global-set-key "\C-z" (lambda () (interactive) (w32-send-sys-command 61696)))

(global-set-key [(control meta delete)] 'backward-kill-sexp)
(global-set-key [(control meta backspace)] 'backward-kill-sexp)

(global-set-key [up] (lambda () (interactive) (scroll-down 1)))
(global-set-key [down] (lambda () (interactive) (scroll-up 1)))

(global-set-key [left] (lambda () (interactive) (scroll-right tab-width t)))
(global-set-key [right] (lambda () (interactive) (scroll-left tab-width t)))

(global-set-key [(shift up)] 'previous-line)
(global-set-key [(shift down)] 'next-line)

(global-set-key [(shift left)] 'backward-char)
(global-set-key [(shift right)] 'forward-char)

;; windmove
(global-set-key [(control left)] 'windmove-left)
(global-set-key [(control right)] 'windmove-right)
(global-set-key [(control up)] 'windmove-up)
(global-set-key [(control down)] 'windmove-down)

(global-set-key [(control meta ?\;)] 'comment-region)

;; Let Natural Keyboard 4000's Zoom buttons cycle buffers.
(global-set-key [(control wheel-up)] 'bs-cycle-previous)
(global-set-key [(control wheel-down)] 'bs-cycle-next)

;; kill buffer without a chord
(global-set-key [(control shift ?k)] '(lambda () (interactive) (kill-buffer (current-buffer))))

;; CVS Emacs is binding this to recenter-top-bottom.
(global-set-key [(control ?l)] 'recenter)
1;2c
;; imenu is useful if it can be invoked quickly
(global-set-key [(control shift ?o)] 'imenu)

;; ace-jump-mode allows you to quickly jump around
(require 'ace-jump-mode)

(when (package-installed-p 'key-chord)
  (require 'key-chord)
  (key-chord-mode 1)
  (key-chord-define-global "jj" 'ace-jump-mode)
  (key-chord-define-global "xo" 'other-window)
  (key-chord-define-global "xb" 'ido-switch-buffer)
  (key-chord-define-global "x0" 'delete-window)
  (key-chord-define-global "x9" 'delete-other-windows)
  (key-chord-define-global "x-" 'split-window-below)
  (key-chord-define-global "x\\" 'split-window-right)
  (setq key-chord-two-keys-delay 0.002))

;; Enable C-o to launch occur while in an isearch, amazingly handy.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))
