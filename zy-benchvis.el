;;; zy-benchvis.el --- Benchmark result visualization -*- lexical-binding: t -*-


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

;; This file extends the benchmarking utility provided by init-benchmark.el of
;; ZyEmacs.  It provides the command `zy-benchvis' which visualizes the
;; benchmark results in a beautiful tree view.

;;; Code:

(require 'init-benchmark)
(require 'zyemacs)


;;;; Benchmark Tree Manipulation

(defun zy-benchvis-root-node-p (node)
  "Return t if NODE is the root node of the benchmark tree."
  (eq node zy/benchmark-tree))

;; Calculate the `taken-adjusted' property for all benchmark nodes

(defun zy-benchvis-sum-nodes-taken (nodes)
  "Return the sum of NODES' taken property."
  (let ((sum 0))
    (mapc (lambda (node)
	    (let ((taken (plist-get (car node) 'taken)))
	      (setq sum (+ sum (if taken taken 0)))))
	  nodes)
    sum))

(defun zy-benchvis-calculate-taken-adjusted (node)
  "Recursively calculate and set NODE's `taken-adjusted' property."
  (let ((taken (plist-get (car node) 'taken)))
    (when taken
      (plist-put (car node) 'taken-adjusted
		 (- taken
		    (zy-benchvis-sum-nodes-taken (cdr node))))))
  (mapc #'zy-benchvis-calculate-taken-adjusted
	(cdr node)))

(zy-benchvis-calculate-taken-adjusted zy/benchmark-tree)

;; Reverse the order of children nodes for more logical display

(defun zy-benchvis-reverse-children (node)
  "Recursively reverse the children nodes of NODE."
  (setcdr node (reverse (cdr node)))
  (mapc #'zy-benchvis-reverse-children (cdr node)))

(zy-benchvis-reverse-children zy/benchmark-tree)


;;;; Tree Printer

(defun zy-benchvis-print-header ()
  "Print the header."
  (insert
   (propertize "Benchmark results" 'face 'font-lock-keyword-face)
   "\n\n"))

(defun zy-benchvis-print-node (padding node)
  "Print PADDING followed by NODE."
  (let ((name (plist-get (car node) 'name))
	(type (plist-get (car node) 'type))
	(since (plist-get (car node) 'since))
	(taken (plist-get (car node) 'taken-adjusted)))
    (insert padding "[")
    (when (and since (numberp since))
      (insert (propertize (format "%.1f ms" since)
			  'face 'font-lock-doc-face)))
    (insert (if since " " "")
	    (propertize (if (stringp name)
			    (format "%s" (file-name-base name))
			  (format "%s" name))
			'face 'font-lock-variable-name-face))
    (when type
      (insert " "
	      (propertize (format "%s" type)
			  'face 'font-lock-type-face)))
    (when (and taken (numberp taken))
      (insert " "
	      (propertize (format "%.1f ms" taken)
			  'face 'font-lock-constant-face)))
    (insert "]\n")))

(defun zy-benchvis-print-nodes (nodes padding)
  "Print the tree nodes NODES after PADDING."
  (let* ((current-node (car nodes))
	 (remaining-nodes (cdr nodes))
	 (children (cdr current-node))
	 (current-padding (concat padding (if remaining-nodes "├─" "╰─")))
	 (subtree-padding (concat padding (if remaining-nodes "│ " "  "))))
    (if (zy-benchvis-root-node-p current-node)
	(zy-benchvis-print-node "╼►" current-node)
      (zy-benchvis-print-node current-padding current-node))
    (when children
      (zy-benchvis-print-nodes children subtree-padding))
    (when remaining-nodes
      (zy-benchvis-print-nodes remaining-nodes padding))))


;;;; The `zy-benchvis' Command

(defvar zy-benchvis-mode-hook nil
  "Hook run when entering `zy-benchvis-mode'.")

(defvar zy-benchvis-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Local keymap for `zy-benchvis-mode' buffers.")

(defun zy-benchvis-setup-buffer ()
  "Setup buffer for the tree view."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays)
    (zy-benchvis-print-header)
    (zy-benchvis-print-nodes (list zy/benchmark-tree) ""))
  (use-local-map zy-benchvis-mode-map)
  (goto-char (point-min)))

(defun zy-benchvis-mode ()
  "Major mode for viewing benchmark results as a tree."
  (kill-all-local-variables)
  (setq buffer-read-only t
	truncate-lines t
	major-mode 'zy-benchvis-mode)
  (zy-benchvis-setup-buffer)
  (run-mode-hooks 'zy-benchvis-mode-hook))

(put 'zy-benchvis-mode 'mode-class 'special)

;;;###autoload
(defun zy-benchvis ()
  "Visualize benchmarking results in a tree view."
  (interactive)
  (let ((buffer-name "*Benchmark Results*"))
    (switch-to-buffer (get-buffer-create buffer-name))
    (unless (eq major-mode 'zy-benchvis-mode)
      (zy-benchvis-mode))))


(provide 'zy-benchvis)

;;; zy-benchvis.el ends here
