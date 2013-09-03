;; local vars
(defvar structed-is-editing nil "local variable, t if structed is in an editing state")
(defvar structed-editing-tree-index nil)
(defvar structed-cur-node-offset nil)
(defvar structed-is-exiting-edit-mode nil)


(defun structed-mode-init-edit ()
  (make-local-variable 'structed-is-editing)
  (make-local-variable 'structed-editing-tree-index)
  (make-local-variable 'structed-cur-node-offset)
  (make-local-variable 'structed-is-exiting-edit-mode)
  (setq inhibit-read-only '(structed-editing-block read-only))
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'structed-handle-after-change))

(defun structed-point-entered-cb (old-point new-point)
  (structed-stop-editing-block))

(defun structed-get-editing-block-properties ()
  "The text properties for the editing block"
  '(point-left structed-point-entered-cb
               point-entered structed-point-entered-cb
               face highlight))

(defun structed-handle-after-change (begin end old-length)
  "Handles insertions/deletions during edit mode"
  (when structed-is-editing
    (when (not (= 0 old-length))
      (when (= -1 (- begin structed-cur-node-offset)) ; Filter out deletions of the leading quote
        (save-excursion (let ((inhibit-read-only t))
                          (goto-char begin)
                          (insert "\"")
                          (add-text-properties
                           begin (+ 1 begin)
                           (structed-get-editing-block-properties))))
        (setq begin (+ 1 begin))
        (setq old-length (- old-length 1)))
      (structed-send-deletions structed-editing-tree-index
                               (- begin structed-cur-node-offset)
                               old-length))
    (when (not (= begin end))
      (add-text-properties begin end (structed-get-editing-block-properties))
      (structed-send-insertions structed-editing-tree-index
                                (- begin structed-cur-node-offset)
                                (buffer-substring begin end)))))

(defun structed-send-deletions (tree-index begin count)
  "Send delete commands to the python process"
  (dotimes (i count)
    (structed-send-command
     `((type . delete)
       (tree_index . ,(reverse-vector tree-index))
       (linear_index . ,begin)))))

(defun structed-send-insertions (tree-index begin chars)
  "Send insert commands to the python process"
  (let ((cursor-pos begin))
    (dolist (ch (remove-empties (split-string chars "")))
      (structed-send-command
       `((type . insert)
         (tree_index . ,(reverse-vector tree-index))
         (linear_index . ,cursor-pos)
         (value . ,ch)))
      (setq cursor-pos (+ 1 cursor-pos)))))

(defun structed-edit-current-node ()
  "Put the current node into edit mode"
  (interactive)
  (structed-hold-local-lock)
  (structed-set-editing-block))

(defun structed-stop-editing-block ()
  "Called when the point moves out of the active editing block"
  (when (and structed-is-editing (not structed-is-exiting-edit-mode))
    (setq structed-is-exiting-edit-mode t)
    (structed-unset-editing-block)
    (structed-release-local-lock)
    (setq structed-is-exiting-edit-mode nil)))

(defun structed-set-editing-block ()
  "Set the node's costraints for editing, and move point there, assumes point is on the line"
  (let ((tree-index (structed-get-current-tree-index)))
    (search-backward-regexp "^\\|\\n")
    (search-forward "\"")
    (let ((quote-block-start (- (point) 1)))
      (search-forward "\"\n")
      (let ((quote-block-end (- (point) 1))
            (inhibit-read-only t))
        (goto-char (+ 1 quote-block-start))
        (add-text-properties
         quote-block-start
         (- quote-block-end 1) 
         '(read-only structed-editing-block))
        (add-text-properties 
         quote-block-start
         quote-block-end
         (structed-get-editing-block-properties))
        (setq structed-editing-tree-index tree-index)
        (setq structed-cur-node-offset (+ 1 quote-block-start))
        (setq structed-is-editing t)))))

(defun structed-unset-editing-block ()
  "Remove the visual styling used on the active editing block"
  (if (not structed-is-editing)
    (progn
      (message "Error: called structed-unset-editing-block when not in edit mode")
      (debug))
    (progn
      (setq structed-is-editing nil)
      (save-excursion
        (goto-char structed-cur-node-offset)
        (search-forward "\"\n")
        (let ((quote-block-start (- structed-cur-node-offset 1))
              (quote-block-end (- (point) 1))
              (inhibit-read-only t))
          (add-text-properties 
           quote-block-start
           (- quote-block-end 1)
           '(read-only t))
          (remove-text-properties
           quote-block-start
           quote-block-end
           (structed-get-editing-block-properties)))))))
