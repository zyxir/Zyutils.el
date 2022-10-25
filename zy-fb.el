;;; zy-fb.el --- File and buffer commands -*- lexical-binding: t -*-


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file provides additional commands about files and buffers for ZyEmacs.

;;; Code:

(require 'zyemacs)

;;;; Open in External App

(defalias 'zy-fb--open-file-externally
  (cond
   ((memq system-type '(ms-dos windows-nt cygwin))
    (lambda (file)
      (w32-shell-execute
       "open"
       (replace-regexp-in-string "/" "\\" file t t))))
   ((memq system-type '(gnu gnu/linux))
    (let ((command (if (zy/wsl-p) "wslview" "xdg-open")))
      (call-process command nil nil nil file)))))

;;;###autoload
(defun zy-fb-open-in-external-app (&optional file)
  "Open some file in external application.

The file to open could be FILE if it is not nil or omitted, or
files marked by Dired in `dired-mode', or the current file.

The app is chosen from the OS's preference."
  (interactive)
  (let* ((flist (cond
                 ((string-equal major-mode "dired-mode")
                  (dired-get-marked-files))
                 ((not file) (list (buffer-file-name)))
                 (file (list file))))
         (do-it (if (<= (length flist) 5)
                    t
                  (y-or-n-p "Open more than 5 files? "))))
    (when do-it
      (mapc 'zy-fb--open-file-externally flist))))


(provide 'zy-fb)

;;; zy-fb.el ends here
