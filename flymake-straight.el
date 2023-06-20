;;; flymake-straight.el --- A Flymake backend that integrate use-package and straight.el -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'cl-lib))

(eval-when-compile
  (require 'subr-x))

(defvar flymake-straight-source-file (if load-in-progress
                                         load-file-name buffer-file-name))

(declare-function lm-get-header-re "lisp-mnt")
(declare-function lm-header-multiline "lisp-mnt")
(declare-function straight-use-package "straight.el")
(declare-function straight--repos-dir "straight.el")
(declare-function straight-use-package-mode "straight.el")
(declare-function package-lint-flymake-setup "package-lint-flymake.el")

(defcustom flymake-straight-ignored-files '(".dir-locals"
                                            "custom"
                                            "filesets-cache"
                                            "company-statistics-cache"
                                            "saveplace" "savehist")
  "List of symbols or files to ignore."
  :group 'flymake-straight
  :type '(repeat (string :tag "File name base")))

(defcustom flymake-straight-use-package-features '(use-package-ensure-system-package)
  "List of additional `use-package' features to require."
  :group 'flymake-straight
  :type '(repeat (symbol :tag "Feature")))

(defcustom flymake-straight-package-lint-predicate '(flymake-straight-user-mail-package-author-p)
  "Whether to enable `package-lint-flymake' in straight repositories.

If the value is a function, it will be called without arguments and should
return non nil if checkdoc should be enabled, othervise - disabled.

If the value a list of functions, all predicates must returns non nil."
  :group 'flymake-straight
  :type '(choice
          (boolean :tag "Start if available" t)
          (function :tag "Function predicate")
          (repeat (function :tag "List of predicates"))))

(defcustom flymake-straight-checkdoc-predicate '(flymake-straight-user-mail-package-author-p)
  "Whether to enable `elisp-flymake-checkdoc' in straight directories.

If the value is a function, it will be called without arguments and should
return non nil if checkdoc should be enabled, othervise - disabled.

If the value a list of functions, all predicates must returns non nil."
  :group 'flymake-straight
  :type '(choice
          (boolean :tag "Start if available" t)
          (function :tag "Function predicate")
          (repeat (function :tag "Function predicate"))))

(defun flymake-straight--batch-compile-for-flymake (&optional file)
  "Helper for `flymake-straight-elisp-flymake-byte-compile'.
Runs in a batch-mode Emacs.  Interactively use variable
`buffer-file-name' for FILE."
  (interactive
   (list buffer-file-name))
  (setq-default straight-use-package-by-default t)
  (require 'straight)
  (straight-use-package 'use-package)
  (straight-use-package-mode t)
  (require 'use-package-core)
  (dolist (sym flymake-straight-use-package-features)
    (require sym nil t))
  (let* ((file
          (or file
              (car command-line-args-left)))
         (coding-system-for-read 'utf-8-unix)
         (collected))
    (setq byte-compile-log-buffer
          (generate-new-buffer " *dummy-byte-compile-log-buffer*"))
    (setq byte-compile-dest-file-function #'ignore)
    (setq byte-compile-log-warning-function
          (lambda
            (string &optional position fill level)
            (push
             (list string position fill level)
             collected)
            t))
    (unwind-protect
        (byte-compile-file file)
      (ignore-errors
        (kill-buffer byte-compile-log-buffer)))
    (prin1 :elisp-flymake-output-start)
    (terpri)
    (pp collected)))

(defvar-local flymake-straight-elisp-flymake--byte-compile-process nil
  "Buffer-local process started for byte-compiling the buffer.")

(defun flymake-straight--flymake--byte-compile-done (report-fn source-buffer
                                                               output-buffer)
  "Return diagnostics for a SOURCE-BUFFER based on OUTPUT-BUFFER.
OUTPUT-BUFFER should containing flymake diagnostics.
Takes three arguments - the reporting function REPORT-FN,
the SOURCE-BUFFER to get diagnostics for, and the
OUTPUT-BUFFER containing the diagnostics."
  (with-current-buffer
      source-buffer
    (save-excursion
      (save-restriction
        (widen)
        (funcall
         report-fn
         (cl-loop with data =
                  (with-current-buffer output-buffer
                    (goto-char (point-min))
                    (search-forward ":elisp-flymake-output-start")
                    (read (point-marker)))
                  for (string pos _fill level) in data
                  do (goto-char pos)
                  for beg = (if (< (point)
                                   (point-max))
                                (point)
                              (line-beginning-position))
                  for end = (min
                             (line-end-position)
                             (or (cdr
                                  (bounds-of-thing-at-point 'sexp))
                                 (point-max)))
                  collect (flymake-make-diagnostic
                           (current-buffer)
                           (if (= beg end)
                               (1- beg) beg)
                           end
                           level
                           string)))))) )

(defun flymake-straight-get-args (temp-file)
  "Return Emacs arguments to compile and check a file TEMP-FILE."
  `(,(expand-file-name invocation-name invocation-directory)
    "--batch"
    ;; "--eval" "(setq load-prefer-newer t)"
    ,@(mapcan (lambda (path)
                (list "-L" path))
              (append (list "./")
                      (seq-remove
                       (lambda (it)
                         (and
                          (string=
                           (file-name-nondirectory
                            (directory-file-name
                             it))
                           "use-package")
                          (not
                           (file-in-directory-p it
                                                user-emacs-directory))))
                       load-path)))
    "-l" ,flymake-straight-source-file
    "-f" "flymake-straight--batch-compile-for-flymake"
    ,temp-file))

(defun flymake-straight-elisp-flymake-byte-compile (report-fn &rest _args)
  "A Flymake backend for elisp files with `use-package' forms with :straight.
Spawn an Emacs process, activate `straight-use-package-mode',
and byte-compiles a file representing the current buffer state and calls
REPORT-FN when done."
  (when flymake-straight-elisp-flymake--byte-compile-process
    (when (process-live-p flymake-straight-elisp-flymake--byte-compile-process)
      (kill-process flymake-straight-elisp-flymake--byte-compile-process)))
  (let ((temp-file
         (make-temp-file "flymake-straight-elisp-flymake-byte-compile"))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min)
                    (point-max) temp-file nil 'nomessage))
    (let* ((output-buffer
            (generate-new-buffer
             " *flymake-straight-elisp-flymake-byte-compile*")))
      (setq
       flymake-straight-elisp-flymake--byte-compile-process
       (make-process
        :name "flymake-straight-elisp-flymake-byte-compile"
        :buffer output-buffer
        :command (flymake-straight-get-args temp-file)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond ((not
                        (and (buffer-live-p source-buffer)
                             (eq proc
                                 (buffer-local-value
                                  'flymake-straight-elisp-flymake--byte-compile-process
                                  source-buffer))))
                       (flymake-log :warning
                                    "flymake straight: byte-compile process %s obsolete"
                                    proc))
                      ((zerop (process-exit-status proc))
                       (flymake-straight--flymake--byte-compile-done report-fn
                                                                     source-buffer
                                                                     output-buffer))
                      (t
                       (funcall report-fn
                                :panic
                                :explanation
                                (format "byte-compile process %s died" proc))))
              (ignore-errors (delete-file temp-file))
              (kill-buffer output-buffer))))
        :stderr " *stderr of flymake-straight-elisp-flymake-byte-compile*"
        :noquery t)))))

(defun flymake-straight-setup-on ()
  "Add `flymake-straight-elisp-flymake-byte-compile' to flymake diagnostic.
Also remove `elisp-flymake-byte-compile' from diagnostic and reactivate
`flymake-mode'."
  (if (bound-and-true-p flymake-mode)
      (flymake-mode -1)
    (require 'flymake))
  (remove-hook 'flymake-diagnostic-functions
               #'elisp-flymake-byte-compile t)
  (add-hook 'flymake-diagnostic-functions
            #'flymake-straight-elisp-flymake-byte-compile nil t)
  (flymake-mode 1))

;;;###autoload
(defun flymake-straight-on ()
  "Add `flymake-straight-elisp-flymake-byte-compile' to flymake diagnostic.
Also remove `elisp-flymake-byte-compile' from diagnostic and reactivate
`flymake-mode'."
  (interactive)
  (flymake-straight-setup-on))

(defun flymake-straight-setup-off ()
  "Remove `flymake-straight-elisp-flymake-byte-compile' from flymake diagnostic.
Also add `elisp-flymake-byte-compile' from diagnostic and reactivate
`flymake-mode'."
  (require 'flymake)
  (if (bound-and-true-p flymake-mode)
      (flymake-mode -1)
    (require 'flymake))
  (remove-hook 'flymake-diagnostic-functions
               #'flymake-straight-elisp-flymake-byte-compile t)
  (add-hook 'flymake-diagnostic-functions
            #'elisp-flymake-byte-compile nil t)
  (flymake-mode 1))

;;;###autoload
(defun flymake-straight-off ()
  "Remove `flymake-straight-elisp-flymake-byte-compile' from flymake diagnostic.
Also add `elisp-flymake-byte-compile' from diagnostic and reactivate
`flymake-mode'."
  (interactive)
  (flymake-straight-setup-off))

(defun flymake-straight-fix-package-lint ()
  "And melpa to package archives and load `package-archive-contents'."
  (progn
    (require 'package)
    (setq-default package-gnupghome-dir nil)
    (unless (bound-and-true-p package-archive-contents)
      (setq package-user-dir (concat (temporary-file-directory) "elpa"))
      (when (boundp 'package-archives)
        (add-to-list 'package-archives
                     '("melpa" . "https://melpa.org/packages/")))
      (package-initialize t)
      (package-refresh-contents t))))

(defun flymake-straight-looks-like-package ()
  "Return non nil whether current buffer has package headers."
  (save-match-data
    (save-excursion
      (save-restriction
        (require 'lisp-mnt)
        (widen)
        (goto-char (point-min))
        (re-search-forward
         (lm-get-header-re (rx (or "Version" "Package-Version"
                                   "Package-Requires")))
         nil t)))))

(defun flymake-straight-all-pass (filters)
  "Create an unary predicate function from FILTERS.
Return t if every one of the provided predicates is satisfied by provided
 argument."
  (not (catch 'found
           (dolist (filter filters)
             (unless (funcall filter)
               (throw 'found t))))))

(defun flymake-straight-user-mail-package-author-p ()
  "Return non nil if `user-mail-address' is listed in package header's author.
If `user-mail-address' is nil, return t."
  (or (not user-mail-address)
      (save-match-data
        (seq-find
         (apply-partially #'string-match-p (regexp-quote
                                            user-mail-address))
         (lm-header-multiline "Author")))))

(defun flymake-straight-in-straight-dir ()
  "Return non if current file is in `straight--repos-dir'."
  (when buffer-file-name
    (file-in-directory-p buffer-file-name (straight--repos-dir))))

(defun flymake-straight-check-predicate (value)
  "Check predicate VALUE."
  (or (eq value t)
      (if (functionp value)
          (funcall value)
        (and value
             (flymake-straight-all-pass
              value)))))

(defun flymake-straight-enable-package-lint ()
  "Disable or enable package-lint.
It depends on the vlaue of `flymake-straight-package-lint-predicate'."
  (when (and (flymake-straight-looks-like-package)
             (flymake-straight-check-predicate
              flymake-straight-package-lint-predicate))
    (flymake-straight-fix-package-lint)
    (require 'package-lint-flymake nil t)
    (package-lint-flymake-setup)))


(defun flymake-straight-configure-checkdoc ()
  "Disable or enable checkdoc depending on `flymake-straight-checkdoc-predicate'."
  (let ((enabled (flymake-straight-check-predicate
                  flymake-straight-checkdoc-predicate)))
    (if (not enabled)
        (remove-hook 'flymake-diagnostic-functions 'elisp-flymake-checkdoc t)
      (when (not (memq 'elisp-flymake-checkdoc flymake-diagnostic-functions))
        (add-hook 'flymake-diagnostic-functions 'elisp-flymake-checkdoc nil t)))))

(defun flymake-straight--elisp-auto-setup ()
  "Enable and setup `flymake-mode' with different backends based on the filename.

In `straight--repos-dir' the function condionally enable or disable
`package-lint-flymake' and `elisp-flymake-checkdoc'.

See custom variables `flymake-straight-checkdoc-predicate' and
`flymake-straight-package-lint-predicate'.

In the `user-emacs-directory' replace `elisp-flymake-byte-compile' with
`flymake-straight-elisp-flymake-byte-compile'."
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (let ((buffname (buffer-name (current-buffer))))
    (cond ((string= buffname "*Pp Eval Output*"))
          ((or (not buffer-file-name)
               (member (file-name-extension buffer-file-name)
                       (list "elc")))
           nil)
          ((flymake-straight-in-straight-dir)
           (flymake-straight-configure-checkdoc)
           (setq-local elisp-flymake-byte-compile-load-path
                       (append
                        elisp-flymake-byte-compile-load-path
                        load-path))
           (flymake-straight-enable-package-lint)
           (flymake-mode 1))
          ((and (file-in-directory-p buffer-file-name user-emacs-directory)
                (not (member (file-name-base buffer-file-name)
                             flymake-straight-ignored-files)))
           (flymake-straight-setup-on)))))

;;;###autoload
(defun flymake-straight-flymake-elisp-mode-init ()
  "Enable and setup `flymake-mode' with different backends based on the filename.

In `straight--repos-dir' the function condionally enable or disable
`package-lint-flymake' and `elisp-flymake-checkdoc'.

See custom variables `flymake-straight-checkdoc-predicate' and
`flymake-straight-package-lint-predicate'.

In the `user-emacs-directory' replace `elisp-flymake-byte-compile' with
`flymake-straight-elisp-flymake-byte-compile'."
  (interactive)
  (flymake-straight--elisp-auto-setup))

(provide 'flymake-straight)
;;; flymake-straight.el ends here
;; Local Variables:
;; after-save-hook: (lambda () (setq flymake-straight-source-file (buffer-file-name)) (eval-buffer))
;; End:
