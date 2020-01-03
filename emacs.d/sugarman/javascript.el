(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json5\\'" . js2-mode))

(setq js2-basic-offset 2
      js2-bounce-indent-flag t
      js2-cleanup-whitespace t
      js2-enter-indents-newline nil
      js2-highlight-level 3
      js2-mirror-mode nil
      js2-rebind-eol-bol-keys nil
      js2-use-font-lock-faces t)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))
; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'js2-mode)
; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")
; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))
; https://github.com/purcell/exec-path-from-shell
; only need exec-path-from-shell on OSX
; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
; use local eslint from node_modules before global
; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun ns/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'ns/use-eslint-from-node-modules)
; Turn off default js2 warnings.
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; prettier-eslint-emacs
;(require 'prettier-eslint)
;(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'prettier-eslint nil t)))

;; js2-ac
(add-to-list 'company-backends 'ac-js2-company)

;; prettier-js
(defun ns/prettier-2 ()
  (interactive)
  (require 'prettier-js)
  (prettier-js-mode)
  (setq ns-skip-prettier t)
  (setq prettier-js-args
        '("--single-quote" "true"
          "--trailing-comma" "es5"
          "--semi" "false")))

(defun ns/prettier-1 ()
  (interactive)
  (require 'prettier-js)
  (prettier-js-mode)
  (setq ns-skip-prettier t)
  (setq prettier-js-args
        '("--single-quote" "true"
          "--trailing-comma" "es5"
          "--print-width" "100")))

;(when (not (bound-and-true-p ns-skip-prettier))
;  (require 'prettier-js)
;  (add-hook 'js2-mode-hook 'prettier-js-mode)
;  (add-hook 'web-mode-hook 'prettier-js-mode)
;  (prettier-1)
;)

(defun ns/toggle-prettier ()
  (interactive)
  (if ns-skip-prettier (ns/disable-prettier) (ns/enable-prettier)))

(defun ns/enable-prettier ()
  (ns/prettier-1)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (prettier-js-mode)
  (setq ns-skip-prettier t))

(defun ns/disable-prettier ()
  (require 'prettier-js)
  (remove-hook 'js2-mode-hook 'prettier-js-mode)
  (remove-hook 'web-mode-hook 'prettier-js-mode)
  (prettier-js-mode 0)
  (setq ns-skip-prettier nil))

(ns/toggle-prettier)

;; sgml
(when (not (bound-and-true-p skip-sgml-mode))
  (load-file (concat elisp-root "./../misc_packages/sgml-mode-patch.el"))
  (require 'sgml-mode))

;; Main
(defun on-javascript-loaded ()
  (delete-selection-mode 1)
  (subword-mode 1)

  ;(require 'angular-mode)
  (define-key js2-mode-map (kbd "C-c C-e") 'flyspell-mode)

  ;(defun ns-js2-indent-bounce ()
  ;  (interactive)
  ;  (setq js2-bounce-indent-p t)
  ;  (js2-indent-line)
  ;  (setq js2-bounce-indent-p nil))
  ;(defun ns-js2-indent-bounce-backwards ()
  ;  (interactive)
  ;  (setq js2-bounce-indent-p t)
  ;  (js2-indent-bounce-backwards)
  ;  (setq js2-bounce-indent-p nil))
  ;(local-set-key (kbd "C-M-i") 'ns-js2-indent-bounce)
  ;(local-set-key (kbd "C-M-i") 'ns-js2-indent-bounce-backwards)

  ; (closure-lint-mode)

  ;(require 'js2-refactor)
  ;(js2r-add-keybindings-with-prefix "C-c C-j")
)

; speed things up (?)
(setq js2-idle-timer-delay .5)
(setq inhibit-compacting-font-caches t)

;; Add Google Closure externs
;(add-hook 'js2-post-parse-callbacks
;          (lambda ()
;            (let ((buf (buffer-string))
;                  (index 0))
;              (while (string-match "\\(goog\\.require\\|goog\\.provide\\)('\\([^'.]*\\)" buf index)
;                (setq index (+ 1 (match-end 0)))
;                (add-to-list 'js2-additional-externs (match-string 2 buf))))))

;; Add jasmine externs
(defun add-jasmine-externs()
  (interactive)
  (when (or (string-match "_spec.js" (buffer-file-name))
            (string-match "_test.js" (buffer-file-name)))
    (add-to-list 'js2-additional-externs
            '("jasmine" "describe" "xdescribe" "it" "xit" "expect" "spyOn"
              "beforeEach" "afterEach" "runs" "waits" "waitsFor"
              "angular" "module" "inject"))))
(add-hook 'js2-mode-hook 'add-jasmine-externs)

(add-hook 'js2-mode-hook 'eslintd-fix-mode)

;(eval-after-load "js2-mode"
;  '(let ((closure-snippets "~/.emacs.d/snippets/closure-snippets/emacs"))
;     (when (file-exists-p closure-snippets)
;       (add-to-list 'load-path closure-snippets)
;       (require 'closure-snippets-support)
;       (yas/load-directory closure-snippets))))

(add-hook 'js2-mode-hook 'on-javascript-loaded)
