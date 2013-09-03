;; local vars
(defvar structed-is-moving nil "local variable, t if structed is in a moving mode state")
(defvar structed-move-src-tree-index nil)
(defvar structed-move-line-start nil)
(defvar structed-move-line-end nil)

(defun structed-mode-init-move ()
  (make-local-variable 'structed-is-moving)
  (make-local-variable 'structed-move-src-tree-index))

(defun structed-move-current-node ()
  "Put the current node into move mode"
  (interactive)
  (structed-hold-local-lock)
  (structed-enter-move-mode))

(defun structed-get-move-block-properties ()
  '(face highlight))

(defun structed-enter-move-mode ()
  (setq structed-move-src-tree-index (structed-get-current-tree-index))
  (if (= 0 (length structed-move-src-tree-index))
      (message "Can't move root")
    (progn
      (setq structed-is-moving t)
      (save-excursion
        (search-backward-regexp "^\\|\\n")
        (when (eq (char-after) "\n")
          (goto-char (+ (point) 1)))
        (setq structed-move-line-start (point))
        (search-forward "\n")
        (goto-char (- (point) 1))
        (setq structed-move-line-end (point))
        (let ((inhibit-read-only t))
          (add-text-properties 
           structed-move-line-start
           structed-move-line-end
           (structed-get-move-block-properties))))
      (message "Move (b)efore or (a)fter a selected line, or (q)uit"))))

(defun structed-mode-move-insert-before ()
  "Insert the moving node before the current one"
  (interactive)
  (structed-move-command 'before structed-move-src-tree-index (structed-get-current-tree-index))
  (structed-mode-move-quit))
   
(defun structed-mode-move-insert-after ()
  "Insert the moving node after the current one"
  (interactive)
  (structed-move-command 'after structed-move-src-tree-index (structed-get-current-tree-index))
  (structed-mode-move-quit))

(defun structed-move-command (move-type src-index dest-index)
  "Tries to apply the move command"
  (cond 
   ((equal src-index dest-index)
    (message "Can't move a node over it's self"))
   ((= 0 (length src-index))
    (message "Can't move root"))
   ((= 0 (length dest-index))
    (message "Can't move over root"))
   (t
    (structed-send-command
     `((type . move)
       (src_tree_index . ,(reverse-vector src-index))
       (dest_tree_index . ,(reverse-vector dest-index))
       (move_type . ,move-type))))))

(defun structed-mode-move-quit ()
  "stop trying to move the current node"
  (interactive)
  (let ((inhibit-read-only t))
    (remove-text-properties 
     structed-move-line-start
     structed-move-line-end
     (structed-get-move-block-properties)))
  (setq structed-is-moving nil)
  (structed-release-local-lock))
