;;; zyutils-management.el --- Configuration management -*- lexical-binding: t -*-


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

;; Provide functions for loading, compiling and testing the ZyEmacs
;; configuration.

;;; Code:

(require 'restart-emacs)


;; Recompile the config

;;;###autoload
(defun zyutils-recompile-config (&optional force)
  "Recompile the ZyEmacs config.

If optional argument FORCE is non-nil, or with prefix argument
when called interactively, force the re-compilation of every
file."
  (interactive "P")
  (let* ((before-recompile-time (current-time))
	 (files (cl-mapcar
		 #'(lambda (dir)
		     (expand-file-name dir user-emacs-directory))
		 '("early-init.el" "init.el")))
	 (loaddefs-file (if (fboundp 'zy/get-loaddefs-file)
			    (zy/get-loaddefs-file)
			  nil))
	 (dirs-files (cl-mapcan
		      #'(lambda (dir)
			  (directory-files
			   (file-name-as-directory
			    (expand-file-name dir user-emacs-directory))
			   nil ".*\\.el\\'" 'nosort))
		      '("lisp")))
	 (files (nconc files dirs-files (if loaddefs-file `(,loaddefs-file) nil)))
	 (byte-compile-verbose nil)
	 should-recompile
	 elc)
    (dolist (file files)
      ;; Determine if the file should be recompiled
      (if force
	  (setq should-recompile t)
	(setq elc (concat file "c"))
	(setq should-recompile
	      (or (not (file-exists-p elc))
		  (and (file-writable-p elc)
		       (file-newer-than-file-p file elc)))))
      (condition-case-unless-debug nil
	  (when should-recompile
	    (message "Recompiling %s..." file)
	    (byte-compile-file file)
	    (when (native-comp-available-p)
	      (native-compile file))
	    (message "Recompiling %s...done" file))
	(error
	 (message "Recompiling %s...failed" file)
	 (when (file-exists-p elc)
	   (delete-file elc)))))
    (message "Recompilation finishes in %f seconds."
	     (float-time (time-since before-recompile-time))))
  t)


;; Launch a new instance of Emacs

;;;###autoload
(defun zyutils-test-config (&optional recompile)
  "Test the updated config.

Start a new instance of Emacs with \"--debug-init\" argument to
test it.

If RECOMPILE is non-nil, or with prefix argument when called
interactively, recompile the whole config before starting the new
instance."
  (interactive "P")
  (save-some-buffers)
  (when recompile
    (zyutils-recompile-config))
  (eval-and-compile (require 'restart-emacs))
  (restart-emacs-start-new-emacs '("--debug-init")))


(provide 'zyutils-management)

;;; zyutils-management.el ends here
