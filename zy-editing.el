;;; zy-editing.el --- Editing enhancements -*- lexical-binding: t -*-


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

;; This file extends init-editing.el, providing more advanced functionalities
;; for text editing.

;;; Code:

(require 'zyemacs)

;;;; Paragraph

(defun zy/unfill-paragraph (&optional region)
  "Unfill a multi-line paragraph.

If REGION is not nil, unfill every paragraph in the region."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (dlet ((fill-column (point-max))
	 (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(provide 'zy-editing)

;;; zy-editing.el ends here
