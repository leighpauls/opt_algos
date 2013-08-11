(require 'json)

(defvar structed-mode-hook nil)

(defvar structed-mode-keymap
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for the Structed Major Mode")

(make-local-variable 'last-swapped-text)
(make-local-variable 'structed-tree)

(defun set-tree-from-json (json-text)
  (setq structed-tree (json-read-from-string json-text))
  (structed-draw-tree))

(defun structed-mode ()
  "Major Mode for using Structed"
  (interactive)
  (kill-all-local-variables)
  (use-local-map structed-mode-keymap)
  (setq major-mode 'structed-mode)
  (setq mode-name "Structed")
  (setq last-swapped-text "DEFAULT_VALUE!!!")
  (setq debug-on-error t)
  (setq structed-tree 
        '((val . "root node")
          (ch . [((val . "child")
                  (ch . [((val . "grandchild")
                          (ch . nil))]))
                 ((val . "second child")
                  (ch . nil))])))
  (run-hooks 'structed-mode-hook))

(defun structed-draw-layer (num-dots subtree)
  (dotimes (n num-dots nil)
    (insert "* "))
  (insert (cdr (assoc 'val subtree)) "\n")
  (let ((new-dots (+ num-dots 1))
        (children (cdr (assoc 'ch subtree))))
    (dotimes (i (length children) nil)
      (structed-draw-layer new-dots (aref children i)))))

(defun structed-draw-tree ()
  "draw the tree over the curret buffer"
  (interactive)
  (save-excursion
    (delete-region (point-min) (point-max))
    (structed-draw-layer 1 structed-tree)))

(defun structed-do-change ()
  "fucking around with some text"
  (interactive)
  (let ((start-pos (min (point) (mark)))
        (end-pos (max (point) (mark))))
    (let ((buffer-text (buffer-substring start-pos end-pos))
          (new-end-pos (+ start-pos (length last-swapped-text))))
      (save-excursion
        (delete-region start-pos end-pos)
        (insert last-swapped-text)
        (setq last-swapped-text buffer-text))
      (goto-char new-end-pos)
      (set-mark start-pos))))

(provide 'structed-mode)
