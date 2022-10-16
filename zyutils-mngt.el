;;; zyutils-mngt.el --- Configuration management -*- lexical-binding: t -*-


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
(defun zyutils-mngt-recompile-config (&optional force)
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
			   'full ".*\\.el\\'" 'nosort))
		      '("lisp")))
	 (files (nconc files dirs-files (if loaddefs-file `(,loaddefs-file) nil)))
	 (byte-compile-verbose nil)
	 should-recompile
	 elc)
    (dolist (file files)
      ;; Determine if the file should be recompiled
      (setq elc (concat file "c"))
      (if force
	  (setq should-recompile t)
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
(defun zyutils-mngt-test-config (&optional recompile)
  "Test the updated config.

Start a new instance of Emacs with \"--debug-init\" argument to
test it.

If RECOMPILE is non-nil, or with prefix argument when called
interactively, recompile the whole config before starting the new
instance."
  (interactive "P")
  (save-some-buffers)
  (when recompile
    (zyutils-mngt-recompile-config))
  (eval-and-compile (require 'restart-emacs))
  (restart-emacs-start-new-emacs '("--debug-init")))


;; Dedicated mode for displaying benchmark result

(unless (boundp 'zy/bench-timetable)
  (defvar zy/bench-timetable nil))

(define-derived-mode zyutils-mngt-benchmark-result-mode
  tabulated-list-mode "Benchmark Result"
  "Show times taken to load each features."
  (setq tabulated-list-format
	[("Start time (ms)" 20 zyutils-mngt--sort-by-since)
	 ("Feature" 30 t)
	 ("Time (ms)" 20 zyutils-mngt--sort-by-taken)])
  (setq tabulated-list-entries #'zyutils-mngt-benchmark-list-entries)
  (tabulated-list-init-header))

(defun zyutils-mngt-get-time-string (time)
  "Get TIME in string format.

TIME will first be converted to float format, then converted to
milliseconds, and then formated to a string."
  (format "%.2f" (* 1000 (float-time time))))

(defun zyutils-mngt-benchmark-list-entries ()
  "Get list entries for `zyutils-mngt-mode'."
  (cl-loop for (since feature taken) in zy/bench-timetable
	   with order = 0
	   do (cl-incf order)
	   collect (list order
			 (vector (zyutils-mngt-get-time-string since)
				 (symbol-name feature)
				 (zyutils-mngt-get-time-string taken)))))

(defun zyutils-mngt--sort-by-since (entry1 entry2)
  "Return t if ENTRY1 has a greater SINCE value than ENTRY2."
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun zyutils-mngt--sort-by-taken (entry1 entry2)
  "Return t if ENTRY1 has a greater TAKEN value than ENTRY2."
  (< (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))


;; Command to show benchmark result

;;;###autoload
(defun zyutils-mngt-show-benchmark-result ()
  "Show a tabular view of startup benchmark result."
  (interactive)
  (with-current-buffer (get-buffer-create "*Benchmark Result*")
    (zyutils-mngt-benchmark-result-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))


(provide 'zyutils-mngt)

;;; zyutils-mngt.el ends here
