; Match whitespace or empty line.
(defun s-empty? (text)
  (s-matches? "^[^a-z^A-Z^0-9]*$" text))

(defun delete-blank-lines-and-trailing-whitespace ()
  (interactive)
  (delete-blank-lines)
  (delete-trailing-whitespace))

(defun directory-parent (directory)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (if (not (equal directory parent))
        parent)))

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

(defun disable-final-newline ()
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

(defun ido-switch-buffer-no-flx ()
  (interactive)
  (flx-ido-mode 0)
  (ido-switch-buffer))

(defun ido-switch-buffer-flx ()
  (interactive)
  (flx-ido-mode 1)
  (ido-switch-buffer))

(defun ido-find-file-no-flx ()
  (interactive)
  (flx-ido-mode 0)
  (ido-find-file))

(defun ido-find-file-flx ()
  (interactive)
  (flx-ido-mode 1)
  (ido-find-file))

(defun save-macro (name)
  "Save last used macro under the given function name at end of"
  "~/.emacs.d/<uid>/macros.el"
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (find-file (concat elisp-root "/macros.el"))
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (save-buffer)
  (switch-to-buffer nil))

(require 's)

(defun to-hyphen-case ()
  (interactive)
  (setq m-bounds (bounds-of-thing-at-point 'symbol))
  (setq m-text (buffer-substring-no-properties (car m-bounds) (cdr m-bounds)))
  (delete-region (car m-bounds) (cdr m-bounds))
  (insert (s-dashed-words m-text)))

; "snippet-support" -> "SnippetSupport"
(defun buffer-class-name ()
  (s-upper-camel-case (first (s-split "\\." (buffer-name)))))

; "snippet-support-test" -> "SnippetSupport"
(defun buffer-nontest-class-name ()
  (s-upper-camel-case (sss-nontest-name)))

; "snippet-support-test" -> "snippet-support"
(defun buffer-nontest-name ()
  (first (s-split "-test"
                  (first (s-split "\\." (buffer-name))))))

; "snippet-support-test" -> "snippet support"
(defun buffer-nontest-desc ()
  (s-join " " (s-split "-" (sss-nontest-name))))

; "functions.el" -> "el"
(defun buffer-extension ()
  (car (last (s-split "\\." (buffer-name)))))

(defun symbol-at-point-no-properties ()
  (setq m-symbol-bounds (bounds-of-thing-at-point 'symbol))
  (if (car m-symbol-bounds)
      (buffer-substring-no-properties
        (car m-symbol-bounds) (cdr m-symbol-bounds))))

(defun highlight-symbol ()
  (interactive)
  (setq m-symbol-bounds (bounds-of-thing-at-point 'symbol))
  (goto-char (car m-symbol-bounds))
  (set-mark (cdr m-symbol-bounds))
  (setq deactivate-mark nil))
