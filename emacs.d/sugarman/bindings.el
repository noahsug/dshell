;; Store common files in registers - open via C-x r j <key>
(set-register ?j (cons 'file "~/.emacs.d/sugarman/javascript.el"))
(set-register ?e (cons 'file "~/.emacs.d/sugarman/bindings.el"))
(set-register ?b (cons 'file "~/.bashrc.d/sugarman/main.sh"))

(global-set-key "\C-xb" 'helm-buffers-list)
(global-set-key "\C-xB" 'ibuffer)
(global-set-key "\C-xn" 'ido-switch-buffer-no-flx)
(global-set-key "\C-x\C-f" 'helm-find-files)
(global-set-key "\C-x\C-n" 'ido-find-file-no-flx)

;(global-set-key "\M-/" 'hippie-expand)
;(global-set-key "\M-i" 'dabbrev-expand)
;(global-set-key "\M-i" 'company-complete)
(global-set-key "\M-i" 'company-abort)

(global-set-key (kbd "C-M-w") 'macro-search-symbol)
; rg search at point from current dir
(global-set-key (kbd "C-M-e") 'rg-dwim)
; jump tp definiton
(global-set-key (kbd "C-M-d") 'dumb-jump-go)

; open binding: "C-x t"

;(global-set-key "\C-x\C-g\C-d" 'delete-selection-mode)
(global-set-key "\C-x\C-g\C-s" 'subword-mode)
(global-set-key "\C-x\C-g\C-o" 'qd-save)
; quick desktop
(global-set-key "\C-x\C-go" 'qd-load)
(global-set-key "\C-x\C-gO" 'qd-delete)

(global-set-key "\C-x\C-g\C-n" 'disable-final-newline)
(global-set-key "\C-x\C-g\C-y" 'yas-reload-all)
(global-set-key "\C-x\C-g\C-m" 'save-macro)
(global-set-key "\C-x\C-g\C-l" 'sort-lines)

; jsx mode
(global-set-key "\C-x\C-g\C-x" 'rjsx-mode)
;(global-set-key "\C-x\C-g\C-f" 'flyspell-mode)
;(global-set-key "\C-x\C-g\C-c" 'flycheck-mode)
(global-set-key "\C-x\C-g\C-c" 'ns/copy-filename)
(global-set-key "\C-x\C-gc" 'ns/copy-file)
(global-set-key "\C-x\C-g\C-p" 'ns/prettier-1)
(global-set-key "\C-x\C-g\p" 'ns/prettier-2)
(global-set-key "\C-x\C-g\P" 'ns/toggle-prettier)
(global-set-key "\C-x\C-g\C-e" 'eslintd-fix-mode)
(global-set-key "\C-x\C-ge" 'eslintd-fix)

(global-set-key "\C-xR" 'rename-file-and-buffer)
(global-set-key "\C-xF" 'helm-codesearch-find-pattern)
(global-set-key "\C-xf" 'helm-codesearch-find-file)
(global-set-key "\C-x\C-z" 'fzf)
(global-set-key "\C-x\C-d" 'neotree)
;(global-set-key "\C-xf" 'helm-browse-project)

(global-set-key "\C-c\C-s" 'ispell-word)
(global-set-key "\C-c\C-e" 'flyspell-mode)

(global-set-key "\C-xao" 'macro-diff-accept-ours)
(global-set-key "\C-xat" 'macro-diff-accept-theirs)

(global-set-key "\C-xac" 'macro-change-log-pr)

(global-set-key "\C-x\C-h" 'help-command)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [f5] 'revert-buffer)
(global-set-key "\M-h" 'backward-kill-word)
(global-set-key "\C-\M-h" 'backward-kill-sexp)
(global-set-key "\C-c\C-h" 'c-hungry-delete-backwards)

(global-set-key "\C-xc" 'compile)

(global-set-key "\C-xm" 'shell)
(global-set-key "\C-xM" 'eshell)

;(global-set-key "\C-x\C-w" 'delete-blank-lines-and-trailing-whitespace)

(require 'fastnav)
(defvaralias 'lazy-highlight-face 'isearch-lazy-highlight)
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
;(global-set-key "\M-y" 'fastnav-sprint-forward)
;(global-set-key "\M-Y" 'fastnav-sprint-backward)
;(global-set-key "\M-s" 'fastnav-jump-to-char-forward)
;(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
;(global-set-key "\M-r" 'fastnav-replace-char-forward)
;(global-set-key "\M-R" 'fastnav-replace-char-backward)
;(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
;(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
;(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
;(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
;(global-set-key "\M-k" 'fastnav-delete-char-forward)
;(global-set-key "\M-K" 'fastnav-delete-char-backward)
;(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
;(global-set-key "\M-M" 'fastnav-mark-to-char-backward)

(require 'expand-region)
(global-set-key "\M-k" 'er/expand-region)

;(require 'hungry-delete)
;(global-hungry-delete-mode)

;(global-set-key [(control ?/)] 'PC-lisp-complete-symbol)

;(global-set-key "\C-z" (lambda () (interactive) (w32-send-sys-command 61696)))

;(global-set-key [(control meta delete)] 'backward-kill-sexp)

(global-set-key [up] (lambda () (interactive) (scroll-down 1)))
(global-set-key [down] (lambda () (interactive) (scroll-up 1)))

(global-set-key [left] (lambda () (interactive) (scroll-right tab-width t)))
(global-set-key [right] (lambda () (interactive) (scroll-left tab-width t)))

(global-set-key [(shift up)] 'previous-line)
(global-set-key [(shift down)] 'next-line)

(global-set-key [(shift left)] 'backward-char)
(global-set-key [(shift right)] 'forward-char)

;; CVS Emacs is binding this to recenter-top-bottom.
(global-set-key [(control ?l)] 'recenter)

;; ace-jump-mode allows you to quickly jump around
(require 'ace-jump-mode)
(global-set-key "\M-y" 'jump-char-forward)
;(global-set-key "\C-\M-j" 'jump-char-backward)
;(global-set-key [(control t)] 'jump-char-forward)
;(global-set-key [(meta t)] 'jump-char-backward)

;; revbufs
(require 'revbufs)
(global-set-key (kbd "C-x r a") 'revbufs)

;; evil
;(require 'evil)
;(global-set-key (kbd "C-M-/") 'evil-jump-to-definiton)
;(global-set-key (kbd "C-M-w") 'evil-search-word-forward)
;(global-set-key (kbd "C-M-q") 'evil-search-word-backward)
;(global-set-key (kbd "M-j") 'evil-jump-backward)
;(global-set-key (kbd "M-q") 'evil-jump-backward)
;(global-set-key (kbd "C-M-j") 'evil-jump-forward)

(require 'goto-last-change)
(global-set-key (kbd "M-q") 'goto-last-change)

;; undo tree
(global-undo-tree-mode)
(global-set-key (kbd "C-M-_") 'undo-tree-redo)
(global-set-key (kbd "M-T") 'undo-tree-redo)
(global-set-key (kbd "M-t") 'undo-tree-redo)
(global-set-key (kbd "C-T") 'undo-tree-visualize)

;; codesearch cindex
(defun codesearch-refresh ()
  (interactive)
  (message "refresh cindex")
  (shell-command "cindex airlab/repos/ Dropbox/programming/code/"))

;; Enable C-o to launch occur while in an isearch, amazingly handy.
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun ns/copy-filename ()
  "Put the current file name on the clipboard"
  (interactive)
  (shell-command
   (concat "echo -n '" (buffer-file-name) "' | pbcopy"))
  )

(defun ns/copy-file ()
  "Put the current file on the clipboard"
  (interactive)
  (shell-command
   (concat "cat " (buffer-file-name) " | pbcopy"))
  )
