(require 'json)

(defvar struced-mode-hook nil)

(defvar structed-mode-keymap
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for the Structed Major Mode")

(defun structed-post-insert-handler ()
  "Handle a charactor insertion"
  (message "char inserted"))

(defun structed-mode ()
  "Major Mode for using Structed"
  (interactive)
  (setq debug-on-error t)
  (kill-all-local-variables)
  (use-local-map structed-mode-keymap)
  (setq major-mode 'structed-mode)
  (setq major-name "Structed")
  (add-hook 'post-self-insert-hook 'structed-post-insert-handler nil t)
  (run-hooks 'structed-mode-hook))

(defun structed-draw-layer (num-dots json-tree)
  "Draw the subtree recursively"
  (dotimes (n num-dots nil)
    (insert "* "))
  (let ((value (cdr (assoc 'value json-tree))))
    (dotimes (i (length value) nil)
      (insert (aref value i))))
  (insert "\n")
  (let ((new-dots (+ num-dots 1))
        (children (cdr (assoc 'children json-tree))))
    (dotimes (i (length children) nil)
      (structed-draw-layer new-dots (aref children i)))))

(defun structed-draw-tree (json-text)
  "Draw the tree to the buffer"
  (let ((json-tree (json-read-from-string json-text))
        (old-point-max (point-max))
        (old-point (point)))
    (save-excursion
      (delete-region (point-min) (point-max))
      (structed-draw-layer 1 json-tree))
    (goto-char (min old-point (point-max)))))

(defun tst-draw ()
  (interactive)
  (structed-draw-tree "{\"children\": [{\"children\": [], \"value\": []}, {\"children\": [], \"value\": []}, {\"children\": [], \"value\": [\"g\"]}, {\"children\": [], \"value\": []}, {\"children\": [], \"value\": []}, {\"children\": [], \"value\": [\"j\"]}, {\"children\": [], \"value\": [\"k\"]}, {\"children\": [], \"value\": []}, {\"children\": [], \"value\": []}, {\"children\": [], \"value\": []}, {\"children\": [{\"children\": [], \"value\": []}], \"value\": []}, {\"children\": [], \"value\": [\"n\", \"u\", \"r\"]}, {\"children\": [], \"value\": []}, {\"children\": [], \"value\": []}, {\"children\": [{\"children\": [], \"value\": []}], \"value\": [\"q\", \"q\"]}], \"value\": [\"m\", \"o\", \"r\", \"l\", \"w\", \"p\", \"k\", \"t\", \"a\", \"w\", \"s\", \"c\", \"p\"]}"))
