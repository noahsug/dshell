;;; 'sug-utils.el -- provides some basic utility functions

;; Copyright (c) 2013 Noah Sugarman <noahsug@gmail.com>
;;
;; Author: Noah Sugarman <noahsug@gmail.com>

;;; Commentary:

;;; Code:

(defun get-file-extension ()
  "Returns the file extension"
  (interactive)
  (string-match "^\\([a-zA-Z_-]+\\)\\.\\(\\w+\\)$" (buffer-name))
  (match-string 2 (buffer-name)))

(defun get-extensionless-file-name ()
  "Returns the file extension"
  (interactive)
  (string-match "^\\([a-zA-Z_-]+\\)\\.\\(\\w+\\)$" (buffer-name))
  (match-string 1 (buffer-name)))

(defun get-base-file-name (&optional testSuffix)
  "Returns the file name without a test suffix, e.g. game_spec.cc returns game"
  (interactive)
  (setq testSuffix (or testSuffix "spec"))
  (replace-regexp-in-string
   (format "_%s$" testSuffix) "" (get-extensionless-file-name)))

(defun string-ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (string-match (concat ending "$") s))

(defun delete-line ()
  "Deletes the current line without putting it into the kill ring."
  (delete-region (line-beginning-position) (line-end-position)))

(defun un-camelcase (s &optional sep &optional start)
  "Convert CamelCase string to lower case with separator."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

(defun camelcase (s &optional sep)
  "Convert lower case string S with seperator SEP to CamelCase"
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s (or sep "_"))) ""))

(defun camelcase-to-var-case (s)
  "Convert CamelCase to variableCase"
  (concat (downcase (substring s 0 1))
          (substring s 1)))

(defun var-case (s &optional sep)
  "Convert lower case string S with seperator SEP to variableCase"
  (camelcase-to-var-case (camelcase s "_")))

(defun replace-line (text)
  "Replace the text in the current line with TEXT."
  (delete-line)
  (insert text))

(defun has-extension (extension)
  "Returns whether the current buffer has the given EXTENSION."
  (string-ends-with (buffer-name) extension))

(provide 'sug-utils)
;;; swap-impl-test.el ends here
