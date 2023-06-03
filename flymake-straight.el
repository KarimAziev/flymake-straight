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

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))

(defvar flymake-straight-source-file (if load-in-progress load-file-name
																			 buffer-file-name))

(defvar flymake-straight-directory (file-name-directory
																		flymake-straight-source-file))



(declare-function straight-use-package "straight.el")
(declare-function straight--repos-dir "straight.el")
(declare-function package-lint-flymake-setup "package-lint-flymake.el")


(defun flymake-straight--batch-compile-for-flymake (&optional file)
	"Helper for `flymake-straight-elisp-flymake-byte-compile'.
Runs in a batch-mode Emacs.  Interactively use variable
`buffer-file-name' for FILE."
	(interactive
	 (list buffer-file-name))
	(setq-default straight-use-package-by-default t)
	(require 'straight)
	(straight-use-package 'use-package)
	(setq-default use-package-verbose t
                use-package-expand-minimally nil
                use-package-compute-statistics t)
	(setq-default straight-use-package-by-default t)
	(require 'straight)
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

(defvar flymake-straight-elisp-flymake-byte-compile-load-path (list "./")
  "Like `load-path' but used by `flymake-straight-elisp-flymake-byte-compile'.
The default value contains just \"./\" which includes the default
directory of the buffer being compiled, and nothing else.")

(put 'flymake-straight-elisp-flymake-byte-compile-load-path 'safe-local-variable
     (lambda (x) (and (listp x) (catch 'tag
                                  (dolist (path x t) (unless (stringp path)
                                                       (throw 'tag nil)))))))

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
        :command `(,(expand-file-name invocation-name invocation-directory)
                   "-Q"
                   "--batch"
                   ,@(mapcan (lambda (path)
                               (list "-L" path))
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
															load-path))
									 "--eval" "(setq load-prefer-newer t)"
									 "-l" ,flymake-straight-source-file
									 "-f" "flymake-straight--batch-compile-for-flymake"
									 ,temp-file)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond ((not
                        (and (buffer-live-p source-buffer)
                             (eq proc
																 (with-current-buffer source-buffer
                                   flymake-straight-elisp-flymake--byte-compile-process))))
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

;;;###autoload
(defun flymake-straight-on ()
	"Add `flymake-straight-elisp-flymake-byte-compile' to flymake diagnostic."
	(interactive)
	(if (bound-and-true-p flymake-mode)
			(flymake-mode -1)
		(require 'flymake))
	(remove-hook 'flymake-diagnostic-functions
							 #'elisp-flymake-byte-compile t)
  (add-hook 'flymake-diagnostic-functions
            #'flymake-straight-elisp-flymake-byte-compile nil t)
  (flymake-mode 1))

;;;###autoload
(defun flymake-straight-off ()
  "Remove `flymake-straight-elisp-flymake-byte-compile' to flymake diagnostic."
  (interactive)
  (require 'flymake)
  (flymake-mode -1)
  (remove-hook 'flymake-diagnostic-functions
               #'flymake-straight-elisp-flymake-byte-compile t)
  (flymake-mode 1))

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

;;;###autoload
(defun flymake-straight-flymake-elisp-mode-init ()
	"Init flymake for `emacs-listp-mode'."
	(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
	(let ((buffname (buffer-name (current-buffer))))
		(cond ((string= buffname "*Pp Eval Output*"))
					((not buffer-file-name) nil)
					((or
						(when (fboundp 'straight--repos-dir)
							(file-in-directory-p buffer-file-name (straight--repos-dir)))
						(when (boundp 'elpaca-repos-directory)
							(file-in-directory-p buffer-file-name
																	 (expand-file-name "melpazoid"
																										 elpaca-repos-directory))))
					 (flymake-straight-fix-package-lint)
					 (require 'package-lint-flymake nil t)
					 (package-lint-flymake-setup)
					 (flymake-mode 1))
					((and (file-in-directory-p buffer-file-name user-emacs-directory)
								(not (member (file-name-base buffer-file-name)
														 '(".dir-locals"
															 "custom"
															 "filesets-cache"
															 "company-statistics-cache"
															 "saveplace" "savehist"))))
					 (setq-local elisp-flymake-byte-compile-load-path
											 (append
												elisp-flymake-byte-compile-load-path
												load-path))
					 (remove-hook 'flymake-diagnostic-functions
												'package-lint-flymake t)
					 (remove-hook 'flymake-diagnostic-functions
												#'elisp-flymake-byte-compile t)
					 (add-hook 'flymake-diagnostic-functions
										 #'flymake-straight-elisp-flymake-byte-compile nil t)
					 (flymake-mode 1))
					(t (remove-hook 'flymake-diagnostic-functions
													'package-lint-flymake t)))))

(provide 'flymake-straight)
;;; flymake-straight.el ends here