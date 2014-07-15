;;; swap-impl-test.el -- Allows for swapping between implementation and its test

;; Copyright (c) 2013 Noah Sugarman <noahsug@gmail.com>
;;
;; Author: Noah Sugarman <noahsug@gmail.com>

;;; Commentary:

;; Assumes a file structure like the following:
;;   implementation path: 'my-project/coffee/game.coffee'
;;   spec path: 'my-project/spec/game_spec.coffee'
;; where 'coffee' and 'spec' can be any file extension or test-suffix respectively.

;; The test or implementation files are created if they don't exist.

;;; Code:

(require 'sug-utils)

(defun is-test-buffer (fileName fileExtension testSuffix)
  "Returns true if the current buffer is a test file."
  (string-equal (format "%s_%s.%s" fileName testSuffix fileExtension) (buffer-name)))

(defun is-impl-buffer (fileName fileExtension testSuffix)
  "Returns true if the current buffer is an implementation file."
  (string-equal (format "%s.%s" fileName fileExtension) (buffer-name)))

(defun get-file-to-swap (fileName fileExtension testSuffix)
  "Find the corresponding test or implementation file."
  (cond
   ((is-test-buffer fileName fileExtension testSuffix)
    (replace-regexp-in-string
     (concat (regexp-quote (format "%s/%s" testSuffix (buffer-name))) "$")
     (format "%s/%s.%s" fileExtension fileName fileExtension)
     (buffer-file-name)))
   ((is-impl-buffer fileName fileExtension testSuffix)
    (replace-regexp-in-string
     (concat (regexp-quote (format "%s/%s" fileExtension (buffer-name))) "$")
     (format "%s/%s_%s.%s" testSuffix fileName testSuffix fileExtension)
     (buffer-file-name)))
   (t nil)))

(defun swap-impl-test (&optional testSuffix)
  "Swap between the implementation file and test file ; create if non-existent.
The default for TESTSUFFIX is 'spec'."
  (interactive)
  (let (fileName fileExtension nextFile)
    ; build the match-string list
    (string-match "^\\([a-zA-Z_]+\\)\\.\\(\\w+\\)$" (buffer-name))
    (setq testSuffix (or testSuffix "spec"))
    (setq fileName (get-base-file-name))
    (setq fileExtension (get-file-extension))
    (setq nextFile (get-file-to-swap fileName fileExtension testSuffix))
    (cond
     ((string-equal nextFile (buffer-file-name))
      (message "can't find test/impl file: invalid file structure"))
     (nextFile
      (find-file nextFile))
     (t
      (message "can't find test/impl file: invalid file name")))))

(provide 'swap-impl-test)
;;; swap-impl-test.el ends here
