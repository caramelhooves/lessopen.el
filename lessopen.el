;;; lessopen.el --- use lesspipe to view files in Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Caramel Hooves
;; SPDX-License-Identifier: GPL-3.0-only
;;
;; Author: Caramel Hooves <caramel.hooves@protonmail.com>
;; Maintainer: Caramel Hooves <caramel.hooves@protonmail.com>
;; Created: Juni 20, 2024
;; Modified: Juni 20, 2024
;; Version: 0.0.1
;; Keywords: convenience tools unix
;; Homepage: https://github.com/caramelhooves/teleport.el
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; (progn (man "less") (Man-goto-section "INPUT PREPROCESSOR"))
;;
;;
;;; Code:

(require 'view)

(defun lessopen--sentinel (process event)
  (let ((done-cb (process-get process :done-cb)))
    (funcall done-cb event)))

(defun lessopen--insert-into-buffer (process output)
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))
    )))

(defun lessopen--open (filename &optional done-cb)
  "Open FILENAME in view-mode. pre-process the file content using LESSOPEN/LESSCLOSE into the current buffer. Call DONE-CB when parsing is done."
  (let ((lessopen (getenv "LESSOPEN"))
        (done-cb (or done-cb (lambda (&rest _args) (message "lessopen: pre-processing %s done" filename)))))
    (cond
     ((string-match "|\\(.*\\)" lessopen)
      (let* ((cmd (list shell-file-name "-c" (format (match-string 1 lessopen) filename)))
             (process (make-process
                      :name "lessopen"
                      :buffer (current-buffer)
                      :stderr nil
                      :sentinel #'lessopen--sentinel
                      :filter #'lessopen--insert-into-buffer
                      :command cmd)))
        (process-put process :done-cb done-cb))))))

(defun view-file-with-lessopen (filename)
  "Open FILENAME in view-mode, pre-process the file content using LESSOPEN/LESSCLOSE."
   (interactive "f")
   (let ((buf (generate-new-buffer (format "%s|less" filename))))
     (switch-to-buffer buf)
     (view-mode)
     (lessopen--open filename)))

(defun lessopen-dired-view-file (prefix)
  "In Dired, examine a file using lesspipe, returning to Dired when done.
When file is a directory, show it in this buffer if it is
inserted. Otherwise, display it in another buffer. If called with
a PREFIX, open the file literally, without lessopen."
  (interactive "P")
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (dired file))
      (if prefix
          (view-file file)
        (view-file-with-lessopen file)))))


(provide 'lessopen)
;;; lessopen.el ends here
