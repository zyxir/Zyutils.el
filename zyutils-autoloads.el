;;; zyutils-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zyutils-management" "zyutils-management.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from zyutils-management.el

(autoload 'zyutils-recompile-config "zyutils-management" "\
Recompile the ZyEmacs config.

If optional argument FORCE is non-nil, or with prefix argument
when called interactively, force the re-compilation of every
file.

\(fn &optional FORCE)" t nil)

(autoload 'zyutils-test-config "zyutils-management" "\
Test the updated config.

Start a new instance of Emacs with \"--debug-init\" argument to
test it.

If RECOMPILE is non-nil, or with prefix argument when called
interactively, recompile the whole config before starting the new
instance.

\(fn &optional RECOMPILE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; zyutils-autoloads.el ends here
