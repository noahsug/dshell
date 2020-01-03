;;
; Options:
;   - "skip-sgml-mode" skips loading 'sgml-mode when true
;;

; Loads everything else.
(setq uid "sugarman")
(setq elisp-root (concat "~/.emacs.d/" uid))

;; Load ELPA packages
(setq load-prefer-newer t)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents (package-refresh-contents))

; copied from ~/.emacs package-selected-packages, with the addition of the first few packages
(setq package-list '(use-package company helm-codesearch magit package-safe-delete helm-flx undo-tree dumb-jump eslintd-fix rg fzf codesearch base16-theme helm-ls-git zenburn-theme ivy exec-path-from-shell json-mode neotree rjsx-mode el-get unbound yasnippet tide rainbow-mode protobuf-mode projectile project-root multiple-cursors markdown-mode key-chord jumpc jump-char hungry-delete haskell-mode haml-mode goto-last-change flx-ido fastnav expand-region elm-mode closure-template-html-mode closure-lint-mode buffer-move auto-complete-c-headers ack-and-a-half ace-jump-mode ac-js2))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(defun load-packages ()
  "Loads packages"
  (interactive)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  (package-list-packages)
)

; Load misc packages
(add-to-list 'load-path "~/.emacs.d/misc_packages")

; Load personalization
(dolist (library (directory-files elisp-root nil "\\.el$"))
  (load (concat elisp-root "/" (file-name-sans-extension library))))
