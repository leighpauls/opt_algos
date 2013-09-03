;;
;; I realize how this front end misses the point of Cortex, but I haven't ported it over to elisp yet.
;;

(require 'json)

;; global vars
(defvar structed-buffer-name "*Structed*"
  "Buffer name to open structed instances in")
(defvar structed-client-buffer-name "*structed-client*")

(defun structed-load-relative (relative-path)
  "Loads an elisp file relatively"
  (load (concat (file-name-directory (or load-file-name buffer-file-name))
                relative-path)))

(structed-load-relative "./structed_keymap.el")
(structed-load-relative "./structed_edit.el")
(structed-load-relative "./structed_move.el")

(defun structed ()
  "Open a new structed buffer"
  (interactive)
  (let ((existing-buffer (structed-find-buffer)))
    (if existing-buffer
        (pop-to-buffer existing-buffer)
      (progn (pop-to-buffer (generate-new-buffer structed-buffer-name))
             (structed-mode)))))

(defun structed-mode ()
  "
Major Mode for using Structed
Basic commands:
 (e)dit the value of the node at point
 (r)emove the node at point
 (a)ppend a new node as a child of the node at point
 (m)ove the node at point

While moving a node, use the commands:
 (a)fter - make the source node a sister node after the node at point
 (b)efore - make the source node a sister node before the node at point
 (q)uit - stop trying to move the node
"
  (interactive)
  (structed-mode-init))

;; TODO: add a hook to take the buffer out of edit/move mode before killing
(defun structed-mode-init ()
  "Initializer for structed mode"
  (kill-all-local-variables)
  (add-text-properties (point-min) (point-max) 
                       '(read-only t))
  (use-local-map (structed-mode-make-keymap))
  (setq major-mode 'structed-mode)
  (setq major-name "Structed")
  (structed-start-process)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'structed-stop-process)
  (add-hook 'kill-buffer-query-functions 
            '(lambda ()
               (structed-stop-editing-block)
               t))
  (structed-mode-init-edit)
  (structed-mode-init-move))

(defun structed-find-buffer ()
  "Look for an existing structed buffer and return it, else nil"
  (dolist (buf (buffer-list) buf)
    (if (with-current-buffer buf
          (and (eq major-mode 'structed-mode)
               (equal structed-buffer-name (buffer-name))))
        (return buf))))

(defun structed-draw-on-output-filter (process output)
  "Accepts the stdout of the structed python client process"
  (with-current-buffer structed-client-buffer-name
    (save-excursion
      (goto-char (point-max))
      (insert output)))
  (unless (or structed-is-editing structed-is-moving)
    (let ((buf (structed-find-buffer)))
      (if buf
          (with-current-buffer buf
            (structed-draw-tree output))
        (progn
          (message "Couldn't find Structed Buffer")
          (kill-process process))))))

(defun structed-start-process ()
  "Starts the background python process"
  (set-process-filter (start-process-shell-command
                       "structed-client"
                       structed-client-buffer-name
                       "python"
                       "/Users/leighpauls/structed_client.py"
                       "2>/dev/null")
                      'structed-draw-on-output-filter))

(defun structed-stop-process () 
  "Stops the background python process"
  (when (get-buffer structed-client-buffer-name)
    (kill-buffer structed-client-buffer-name))
  t)

(defun structed-draw-layer (num-dots json-tree)
  "Draw the subtree recursively"
  (let ((inhibit-read-only t))
    (dotimes (n num-dots nil)
      (insert "* "))
    (insert "\"")
    (let ((value (cdr (assoc 'value json-tree))))
      (dotimes (i (length value) nil)
        (insert (aref value i))))
    (insert "\"\n")
    (let ((new-dots (+ num-dots 1))
          (children (cdr (assoc 'children json-tree))))
      (dotimes (i (length children) nil)
        (structed-draw-layer new-dots (aref children i))))))

(defun structed-draw-tree (json-text)
  "Draw the tree to the buffer"
  (let ((json-tree (json-read-from-string json-text))
        (old-point-max (point-max))
        (old-point (point))
        (inhibit-read-only t))
    (save-excursion
      (delete-region (point-min) (point-max))
      (structed-draw-layer 1 json-tree))
    ;; TODO: figure out how to make the first char read-only
    (add-text-properties (point-min) (point-max) 
                         '(read-only t))
    (goto-char (min old-point (point-max)))))

(defun structed-get-line-depth (line)
  "Get how deep this line is"
  (- (/ (string-match "\\\"" line) 2) 1))

(defun remove-empties (strings)
  (let ((res (list)))
    (dolist (str strings)
      (unless (= 0 (length str))
        (setq res (append res (list str)))))
    res))

(defun list-to-vector (src-list)
  (eval (append (list 'vector) src-list)))

(defun structed-get-current-tree-index ()
  "Return the current index of the pointer"
  (interactive)
  (save-excursion
    (re-search-forward "\n")
    (list-to-vector 
     (structed-do-get-line-depth
      (remove-empties (split-string (buffer-substring
                                     (point-min)
                                     (point))
                                    "\n"))))))

(defun structed-do-get-line-depth (preceding-lines)
  (let ((tree-index (list)))
    (dolist (line (cdr preceding-lines) tree-index)
      (let ((line-depth (structed-get-line-depth line))
            (scan-depth (length tree-index)))
        (cond
         ((= line-depth scan-depth)
          ;; increment the last element of the index
          (setq tree-index (append (list (+ 1 (car tree-index)))
                                   (cdr tree-index))))
         ((> line-depth scan-depth)
          ;; add a new level
          (setq tree-index (append (list 0)
                                   tree-index)))
         (t
          ;; remove a level and increment
          (setq tree-index (cdr tree-index))
          (setq tree-index (append (list (+ 1 (car tree-index)))
                                   (cdr tree-index)))))))))

(defun reverse-vector (orig-vector)
  "Return the vector in reverse order"
  (vconcat (reverse (append orig-vector nil))))

(defun structed-send-command (command-struct)
  "Send command struct (json-encodable) to the python runtime"
  (process-send-string 
   structed-client-buffer-name
   (concat (json-encode command-struct) "\n")))

(defun structed-append-current-node ()
  "Append a child to the currently selected node"
  (interactive)
  (structed-send-command 
   `((type . append)
     (tree_index . ,(reverse-vector (structed-get-current-tree-index))))))

(defun structed-remove-current-node ()
  "Remove the currently selected node"
  (interactive)
  (let ((tree-index (structed-get-current-tree-index)))
    (if (= 0 (length tree-index))
        (message "Can't delete the root of the tree!")
      (structed-send-command
       `((type . remove)
         (tree_index . ,(reverse-vector tree-index)))))))

(defun structed-hold-local-lock ()
  (structed-send-command '((type . hold_local_lock))))
(defun structed-release-local-lock ()
  (structed-send-command '((type . release_local_lock))))

(defun structed-show-cur-pos ()
  (interactive)
  (message "Point at: %s" (reverse-vector (structed-get-current-tree-index))))
