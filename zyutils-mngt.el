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


;; Loaddef collector

;;;###autoload
(defun zyutils-collect-loaddefs (file)
  "Collect all and load path and loaddefs into a single file FILE."
  (message "Generating single big loaddefs file.")
  ;; Populate `load-path' with borg
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (eval-and-compile (require 'borg))
  (borg-initialize)
  ;; Delete FILE if it already exists
  (when (file-exists-p file)
    (delete-file file))
  ;; Generate the single loaddefs file
  (condition-case-unless-debug e
      (with-temp-file file
	(setq-local coding-system-for-write 'utf-8)
	(let ((standard-output (current-buffer))
	      (print-quoted t)
	      (print-level nil)
	      (print-length nil)
	      (home (expand-file-name "~"))
	      the-load-path
	      drones-path
	      autoloads-file
	      loaddefs-file)
	  (insert ";; -*- lexical-binding: t; coding: utf-8; no-native-compile: t -*-\n"
                  ";; This file is generated from enabled drones.\n")
	  ;; Collect drones' path and full `load-path'
	  (dolist (path load-path)
	    (when (string-prefix-p (expand-file-name user-emacs-directory)
				   (expand-file-name path))
	      (push path drones-path))
	    (if (string-prefix-p home path)
		(push (concat "~" (string-remove-prefix home path)) the-load-path)
	      (push path the-load-path)))
	  (push (expand-file-name "lisp" user-emacs-directory) the-load-path)
	  (setq the-load-path (delete-dups the-load-path))
	  (setq the-load-path (cl-remove-if #'(lambda (path)
						(or
						 (file-equal-p path user-emacs-directory)
						 (not (file-exists-p path))))
					    the-load-path))
	  (prin1 `(set `load-path ',(nreverse the-load-path)))
	  (insert "\n")
	  ;; Insert all clone's autoloads.el and loaddefs.el to this file
	  (dolist (path drones-path)
	    (when (file-exists-p path)
	      (setq autoloads-file (car (directory-files path 'full ".*-autoloads.el\\'"))
		    loaddefs-file (car (directory-files path 'full ".*-loaddefs.el\\'")))
	      (when (and autoloads-file
			 (file-exists-p autoloads-file))
		(insert-file-contents autoloads-file))
	      (when (and loaddefs-file
			 (file-exists-p loaddefs-file))
		(insert-file-contents loaddefs-file))))
	  ;; Remove all #$ load cache
	  (goto-char (point-min))
	  (while (re-search-forward "\(add-to-list 'load-path.*#$.*\n" nil t)
	    (replace-match ""))
	  (goto-char (point-min))
	  (while (re-search-forward "\(add-to-list 'load-path.*\n.*#$.*\n" nil t)
	    (replace-match ""))
	  ;; Write local variables region
	  (goto-char (point-max))
	  (insert "\n"
		  "\n(provide 'init-loaddefs)"
                  "\n;; Local Variables:"
                  "\n;; version-control: never"
                  "\n;; no-update-autoloads: t"
                  "\n;; End:"
		  ))
	t)
    (error (delete-file file)
	   (signal 'zy/collect-loaddefs-error (list file e)))))


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
(defun zyutils-test-config (&optional recompile)
  "Test the updated config.

Start a new instance of Emacs with \"--debug-init\" argument to
test it.

If RECOMPILE is non-nil, or with prefix argument when called
interactively, recompile the whole config before starting the new
instance."
  (interactive "P")
  (if (require 'restart-emacs nil 'noerror)
      (progn
	(save-some-buffers)
	(when recompile
	  (zyutils-recompile-config))
	(eval-and-compile (require 'restart-emacs))
	(restart-emacs-start-new-emacs '("--debug-init")))
    (error "You have to install Restart-emacs to use this command!")))


;; Dedicated mode for displaying benchmark result

(unless (boundp 'zy/bench-timetable)
  (defvar zy/bench-timetable nil))

(define-derived-mode zyutils-benchmark-result-mode
  tabulated-list-mode "Benchmark Result"
  "Show times taken to load each features."
  (setq tabulated-list-format
	[("Start time (ms)" 20 zyutils--sort-by-since)
	 ("Feature" 30 t)
	 ("Time (ms)" 20 zyutils--sort-by-taken)])
  (setq tabulated-list-entries #'zyutils-benchmark-list-entries)
  (tabulated-list-init-header))

(defun zyutils-get-time-string (time)
  "Get TIME in string format.

TIME will first be converted to float format, then converted to
milliseconds, and then formated to a string."
  (format "%.2f" (* 1000 (float-time time))))

(defun zyutils-benchmark-list-entries ()
  "Get list entries for `zyutils-mode'."
  (cl-loop for (since feature taken) in zy/bench-timetable
	   with order = 0
	   do (cl-incf order)
	   collect (list order
			 (vector (zyutils-get-time-string since)
				 (symbol-name feature)
				 (zyutils-get-time-string taken)))))

(defun zyutils--sort-by-since (entry1 entry2)
  "Return t if ENTRY1 has a greater SINCE value than ENTRY2."
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun zyutils--sort-by-taken (entry1 entry2)
  "Return t if ENTRY1 has a greater TAKEN value than ENTRY2."
  (< (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))


;; Command to show benchmark result

;;;###autoload
(defun zyutils-show-benchmark-result ()
  "Show a tabular view of startup benchmark result."
  (interactive)
  (with-current-buffer (get-buffer-create "*Benchmark Result*")
    (zyutils-benchmark-result-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))


(provide 'zyutils-mngt)

;;; zyutils-mngt.el ends here
