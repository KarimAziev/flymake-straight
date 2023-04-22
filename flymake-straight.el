;;; flymake-straight.el --- Configure flymake -*- lexical-binding: t -*-

;; Copyright (C) Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/flymake-straight
;; Version: 0.1.0
;; Keywords: lisp local
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Used Packages

;; A Flymake backend for elisp files with `use-package' forms with :straight.

;;; Code:




(defun flymake-straight-elisp-flymake-byte-compile (report-fn &rest _args)
  "A Flymake backend for elisp files with `use-package' forms with :straight.
Spawn an Emacs process, activate `straight-use-package-mode',
and byte-compiles a file representing the current buffer state and calls
REPORT-FN when done."
  (when elisp-flymake--byte-compile-process
    (when (process-live-p elisp-flymake--byte-compile-process)
      (kill-process elisp-flymake--byte-compile-process)))
  (let ((temp-file (make-temp-file "elisp-flymake-byte-compile"))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min)
                    (point-max) temp-file nil 'nomessage))
    (let* ((output-buffer
            (generate-new-buffer " *elisp-flymake-byte-compile*")))
      (setq
       elisp-flymake--byte-compile-process
       (make-process
        :name "elisp-flymake-byte-compile"
        :buffer output-buffer
        :command `(,(expand-file-name invocation-name invocation-directory)
                   "-Q"
                   "--batch"
                   ,@(mapcan (lambda (path)
                               (list "-L" path))
                             elisp-flymake-byte-compile-load-path)
                   "--eval"
                   ,(concat "(progn "
                            "(defun elisp-flymake--batch-compile-for-flymake (&optional file) (interactive (list buffer-file-name)) (setq-default straight-use-package-by-default t) (require 'straight) (straight-use-package 'use-package) (setq-default straight-use-package-by-default t) (require 'straight) (require 'transient) (let* ((file (or file (car command-line-args-left))) (coding-system-for-read 'utf-8-unix) (collected)) (setq byte-compile-log-buffer (generate-new-buffer \" *dummy-byte-compile-log-buffer*\")) (setq byte-compile-dest-file-function #'ignore) (setq byte-compile-log-warning-function (lambda (string &optional position fill level) (push (list string position fill level) collected) t)) (unwind-protect (byte-compile-file file) (ignore-errors (kill-buffer byte-compile-log-buffer))) (prin1 :elisp-flymake-output-start) (terpri) (pp collected)))"
                            "(elisp-flymake--batch-compile-for-flymake "
                            (prin1-to-string
                             temp-file)
                            " )"
                            ")"))
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond ((not
                        (and (buffer-live-p source-buffer)
                             (eq proc (with-current-buffer source-buffer
                                        elisp-flymake--byte-compile-process))))
                       (flymake-log :warning
                                    "byte-compile process %s obsolete" proc))
                      ((zerop (process-exit-status proc))
                       (elisp-flymake--byte-compile-done report-fn
                                                         source-buffer
                                                         output-buffer))
                      (t
                       (funcall report-fn
                                :panic
                                :explanation
                                (format "byte-compile process %s died" proc))))
              (ignore-errors (delete-file temp-file))
              (kill-buffer output-buffer))))
        :stderr " *stderr of elisp-flymake-byte-compile*"
        :noquery t)))))

;;;###autoload
(defun flymake-straight-on ()
  "Add `flymake-straight-elisp-flymake-byte-compile' to flymake diagnostic."
  (interactive)
  (require 'flymake)
  (add-hook 'flymake-diagnostic-functions
            #'flymake-straight-elisp-flymake-byte-compile nil t)
  (flymake-start))

;;;###autoload
(defun flymake-straight-off ()
  "Remove `flymake-straight-elisp-flymake-byte-compile' to flymake diagnostic."
  (interactive)
  (require 'flymake)
  (flymake-mode -1)
  (remove-hook 'flymake-diagnostic-functions
               #'flymake-straight-elisp-flymake-byte-compile t)
  (flymake-mode 1))


(provide 'flymake-straight)
;;; flymake-straight.el ends here