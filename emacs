(package-initialize)

;; Whether you want prettier on by default.
(setq ns-skip-prettier t)

(load "~/.emacs.d/sugarman_init")

;; Codesearch
(defun codesearch-build-index-code ()
  (interactive)
  (codesearch-build-index "/Users/noah_sugarman/Dropbox/programming/code"))
(codesearch-build-index-code)
(global-set-key "\C-x\C-gfc" 'codesearch-build-index-code)

(custom-set-variables
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(css-indent-offset 2)
 '(delete-selection-mode t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-fuzzy-matching-highlight-fn (quote helm-flx-fuzzy-highlight-match) t)
 '(helm-fuzzy-sort-fn (quote helm-flx-fuzzy-matching-sort) t)
 '(helm-locate-fuzzy-match t)
 '(helm-ls-git-fuzzy-match t)
 '(ido-use-faces nil)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-highlight-level 3)
 '(package-selected-packages
   (quote
    (js2-mode company-tabnine company magit package-safe-delete helm-flx undo-tree dumb-jump eslintd-fix rg fzf codesearch base16-theme helm-ls-git zenburn-theme ivy exec-path-from-shell json-mode neotree rjsx-mode el-get unbound yasnippet tide rainbow-mode protobuf-mode projectile project-root multiple-cursors markdown-mode key-chord jumpc jump-char hungry-delete haskell-mode haml-mode goto-last-change flx-ido fastnav expand-region elm-mode closure-template-html-mode closure-lint-mode buffer-move auto-complete-c-headers ack-and-a-half ace-jump-mode ac-js2)))
 '(typescript-indent-level 2 t)
 '(whitespace-line-column 100)
)
