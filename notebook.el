;;; notebook.el --- (WIP) support for Jupyter notebooks in Emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andrew De Angelis

;; Author: Andrew De Angelis <bobodeangelis@gmail.com>
;; Keywords: convenience, languages

;; This program is free software; you can redistribute it and/or modify
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

;; see README.org

;;; Code:

(require 'json)

;; TODO implement these functions
(defun enb-toggle-cell-type (&rest args)
  "Toggle the type of current cell between markdown and code")

(defun enb-move-cell (&rest args)
  "Move the cell up or down depending on ARGS")

(defun enb-insert-cell (&rest args)
  "Insert a new cell")

(defun enb-delete-cell (&rest args)
  "Delete the current cell")

(defvar enb-actions
  ;; TODO rather than using emojis, integrate with all-the-icons, and
  ;; provide pure-text alternatives
  (list
   (buttonize "[md/py]" #'enb-toggle-cell-type)
   (buttonize "  ⬆️  " #'enb-move-cell 'up)
   (buttonize "  ⬇️  " #'enb-move-cell 'down)
   (buttonize " ➕⬆️ " #'enb-insert-cell 'above)
   (buttonize " ➕⬇️ " #'enb-insert-cell 'below)
   (buttonize "  🗑️  " #'enb-delete-cell))
  "Actions to display next to a cell")

(defvar enb-formatted-actions
  (propertize
   (concat
    (char-to-string ?\N{U+200F})
    (string-join enb-actions " ")
    "\n")
   'face 'minibuffer-prompt)
  "`enb-actions' formatted into the propertized string to insert in the buffer")

(defun enb-concat-vector-into-string (vec)
  (cl-loop for line across vec
           ;; do (message "line: %s" line)
           concat line into str
           finally return str))

(defun enb-get-cell-outputs (cell)
  (if-let ((outputs (alist-get 'outputs cell))
           ;; TODO find examples with multiple outputs and loop
           ;; through them
           (non-empty (> (length outputs) 0))
           (first (aref outputs 0))
           ;; TODO generalize this to all possible data types
           (text (or (alist-get 'text first)
                     (alist-get 'text-plain (alist-get 'data
                                                       first)))))
      (enb-concat-vector-into-string text)
    ""))

(defun enb-get-cell-contents (cell)
  "Construct a string from CELL's source.
Add delimiters specifying the cell's contents"
  ;; TODO: this delimiters approach integrates well with markdown, but
  ;; we could probably eventually transition to a better approach with
  ;; invisible text that is parsed by tree-sitter. In particular, the
  ;; backticks are redundant (we already have cell-delimiter
  ;; overlays), and should be done away with
  (let ((delimiters (if  (equal "markdown"
                                (alist-get 'cell_type cell))
                        '("# markdown cell\n" "\n")
                      '("# code cell\n```python\n" "\n```\n")))
        (source (enb-concat-vector-into-string (alist-get 'source cell)))
        (outputs (enb-get-cell-outputs cell)))
    (concat (car delimiters)
            source
            (cadr delimiters)
            outputs)
    ;; TODO output should be a special case that is displayed UNDER
    ;; the cell is output `raw'? if so let's put in fundamental mode
    ))

;; this function is just for testing for now, not sure it'll be useful
;; as a feature
(defun enb-cleanup-ovs ()
  "Helper function for testing, removes all overlays from the current buffer"
  (interactive)
  (remove-overlays (point-min) (point-max)))

(defun enb-next-delimiter (i overlays)
  "Return the start of the next overlay, or `point-max' when there's
no next overlay"
  (if-let ((ov (nth i overlays)))
      (overlay-start ov)
    (point-max)))

(defun enb-get-all-contents ()
  "Read the contents of the current buffer and encode them into a JSON object"
  (let* ((all-overlays (overlays-in (point-min) (point-max)))
         (overlays (seq-filter
                    (lambda (ov)
                      (equal (overlay-get ov 'category) 'emacs-notebook))
                    all-overlays)))
    ;; (message "(length overlays): %s" (length overlays))
    (cl-loop for i from 0 to (1- (length overlays))
             vconcat
             (let* ((this-ov (nth i overlays))
                    (block (buffer-substring-no-properties
                            (overlay-end this-ov)
                            (enb-next-delimiter (1+ i) overlays)))
                    (display (overlay-get this-ov 'display))
                    (props (get-text-property 0
                                              'notebook-properties
                                              display)))
               (list
                (if (not (string-empty-p block))
                    `((source . ,(string-split block "\n"))
                      ,@props)
                  props)))
             into cells
             finally return
             
             (progn
               ;; (message "%s"`((cells . ,cells)))
               (json-encode `((cells . ,cells)))
               )
             )))

(defun ipynb-save ()
  "Save the current buffer into a ipynb file"
  (interactive)
  (let ((contents (enb-get-all-contents)))
    ;; TODO add check to ensure file hasn't been edited since last
    ;; time
    ;; also maybe we shouldn't go by buffer name but some other
    ;; property that would be less liable to get edited by user/emacs
    (find-file (concat
                (replace-regexp-in-string "^\\*\\|\\*$" ""
                                          (buffer-name))
                ".ipynb"))
    (erase-buffer)
    (insert contents)
    ;; (save-buffer)
    ;; (kill-buffer)
    ))


(defun ipynb-render-buffer ()
  "Render the notebook's JSON object into a markdown buffer"
  (let* ((nb-contents (json-read-from-string
                       (buffer-substring-no-properties
                        (point-min)
                        (point-max)))))
    ;; set this buffer to JSON mode and move to the new buffer
    (json-mode)
    (switch-to-buffer (concat "*" (file-name-base) "*")
                      nil 'same-window)
    ;; (erase-buffer)
    (goto-char (point-min))
    ;; (message "(length of cells: %s" (length (alist-get 'cells
    ;;                                                    nb-contents)))
    (setq cells (alist-get 'cells
                           nb-contents))

    (cl-loop for cell across (alist-get 'cells
                                        nb-contents)
             do
             (save-excursion
               (insert (enb-get-cell-contents cell)))
             (let ((ov (make-overlay (point)
                                     (search-forward-regexp "# .* cell"))))
               (overlay-put
                ov 'display
                (propertize
                 enb-formatted-actions
                 'notebook-properties (delq (assoc 'source cell)
                                            cell)))
               (overlay-put ov 'category 'emacs-notebook))
             (goto-char (point-max))
             )
    (goto-char (point-min)))
  ;; TODO figure out a way to connect `buffer-modified-p' to the
  ;; status of the buffer and the file
  (ipynb-mode))

(define-derived-mode ipynb-mode markdown-mode "ipynb"
  "Major mode for editing python notebooks."
  (define-key ipynb-mode-map (kbd "C-x C-s") #'ipynb-save))

(define-derived-mode json-ipynb-mode json-mode "json-ipynb"
  "Major mode for rendering python notebooks."
  (ipynb-render-buffer))

(push '("\\.ipynb\\'" . json-ipynb-mode) auto-mode-alist)


(provide 'notebook)
;;; notebook.el ends here
