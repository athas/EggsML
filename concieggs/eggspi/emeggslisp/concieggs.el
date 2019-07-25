;;; concieggs.el --- concieggs utilities  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Use this for your concieggs Emacs Lisp commands.

;;; Code:

;; Load all installed packages.

(defun run-on-stdin (fun)
  "Run the given function FUN on standard in."
  (let (line)
    (while (setq line (ignore-errors (read-from-minibuffer "")))
      (princ (funcall fun line))
      (terpri))))

(provide 'concieggs)
;;; concieggs.el ends here
