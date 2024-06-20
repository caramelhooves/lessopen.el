;;; lesspipe.el --- use lesspipe to view files in Emacs -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; (progn (man "less") (Man-goto-section "INPUT PREPROCESSOR"))
;;
;;
;;; Code:

(defun lessopen--call (lessopen input)
  (let ((lessopen (s-split-words S))
  (call-process "/home/lexa/config/lesspipe.sh" nil `(,buf t) t "/home/lexa/tegra194-p3668-0000-p3509-0000.dtb")

  )

(defun lessopen--open (input)
  (let ((lessopen (getenv "LESSOPEN"))
        (buf (generate-new-buffer (format "%s|less" input))))

    (switch-to-buffer buf)
    (with-current-buffer buf
      (cond
       ((string-match "||\\(.*\\)" lessopen)
        (start-process-shell-command "lessopen" buf (format (match-string 1 lessopen) input)))
       ((string-match "|\\(.*\\)" lessopen)
        (start-process-shell-command "lessopen" buf (format (match-string 1 lessopen) input)))
       (lessopen
        (let ((output
               (shell-command-to-string (format lessopen input))))
          (if (string= "" output)
              (insert-file-contents-literally input)
            (insert-file-contents-literally output)))))
      (if-let ((lessclose (getenv "LESSCLOSE")))
          (start-process-shell-command "lessclose" buf lessclose))
      (view-mode))))

(defun find-file-with-lessopen (filename)
  "Open FILENAME in view-mode, pre-process the file content using LESSOPEN/LESSCLOSE"
   (interactive "f")
   (lessopen--open filename))

(provide 'lesspipe)
;;; lesspipe.el ends here
