(require 'json)

;; global vars
(defvar struced-mode-hook nil)
(defvar structed-buffer-name "*Structed*"
  "Buffer name to open structed instances in")
(defvar structed-client-buffer-name "*structed-client*")

;; local vars
(defvar structed-is-editing nil)

(defun structed ()
  "Open a new structed buffer"
  (interactive)
  (let ((existing-buffer (structed-find-buffer)))
    (if existing-buffer
        (pop-to-buffer existing-buffer)
      (progn (pop-to-buffer (generate-new-buffer structed-buffer-name))
             (structed-mode-init)))))

(defun structed-mode ()
  "Major Mode for using Structed"
  (interactive)
  (structed-mode-init))

;; TODO: add a hook to take the buffer out of edit/move mode before killing
(defun structed-mode-init ()
  "Initializer for structed mode"
  (kill-all-local-variables)
  ;; (setq buffer-read-only t)
  (make-local-variable 'structed-is-editing)
  ;; (make-local-variable 'inhibit-read-only)
  (add-text-properties (point-min) (point-max) 
                       '(read-only t))
  (setq inhibit-read-only '(structed-editing-block read-only))
  (use-local-map (structed-mode-make-keymap))
  (setq major-mode 'structed-mode)
  (setq major-name "Structed")
  (run-hooks 'structed-mode-hook)
  (structed-start-process)
  (add-hook 'kill-buffer-query-functions 'structed-stop-process))


(defun structed-define-non-edit-key (map key function)
  "Adds the key for non-editing scenarios"
  (define-key map key `(lambda ()
                         (interactive)
                         (if structed-is-editing
                             (insert ,key)
                           (,function)))))

(defun structed-mode-make-keymap ()
  "Make the keymap for the Structed Major Mode"
  (let ((map (make-keymap)))
    (structed-define-non-edit-key map "a" 'structed-append-current-node)
    (structed-define-non-edit-key map "r" 'structed-remove-current-node)
    (structed-define-non-edit-key map "e" 'structed-edit-current-node)
    (define-key map (kbd "C-m") 'next-line)
    map))

(defun structed-find-buffer ()
  "Look for an existing structed buffer and return it, else nil"
  (dolist (buf (buffer-list) buf)
    (if (with-current-buffer buf
          (and (eq major-mode 'structed-mode)
               (equal structed-buffer-name (buffer-name))))
        (return buf))))

(defun structed-draw-on-output-filter (process output)
  "Accepts the stdout of the structed python client process"
  ;; (message "structed got: %s" output)
  (with-current-buffer structed-client-buffer-name
    (save-excursion
      (goto-char (point-max))
      (insert output)))
  (let ((buf (structed-find-buffer)))
    (if buf
        (with-current-buffer buf
          (structed-draw-tree output))
      (progn
        (message "Couldn't find Structed Buffer")
        (kill-process process)))))

(defun structed-start-process ()
  "Starts the background python process"
  (set-process-filter (start-process-shell-command
                       "structed-client"
                       structed-client-buffer-name
                       "python"
                       "/Users/leighpauls/structed_client.py"
                       "2>/dev/null")
                      'structed-draw-on-output-filter)
  (make-local-variable 'kill-buffer-query-functions))

(defun structed-stop-process () 
  "Stops the background python process"
  (kill-buffer structed-client-buffer-name))

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

(defun structed-send-command (command-struct)
  "Send command struct (json-encodable) to the python runtime"
  (process-send-string 
   structed-client-buffer-name
   (concat (json-encode command-struct) "\n")))

(defun structed-append-root-node ()
  "Dummy function to append a child to the root node"
  (interactive)
  (structed-send-command '(:type "append" :tree_index [])))


(defun structed-append-current-node ()
  "Append a child to the currently selected node"
  (interactive)
  (structed-send-command 
   `((type . append)
     (tree_index . ,(structed-get-current-tree-index)))))

(defun structed-remove-current-node ()
  "Remove the currently selected node"
  (interactive)
  (let ((tree-index (structed-get-current-tree-index)))
    (if (= 0 (length tree-index))
        (message "Can't delete the root of the tree!")
      (structed-send-command
       `((type . remove)
         (tree_index . ,tree-index))))))


(defun structed-hold-local-lock ()
  (structed-send-command '((type . hold_local_lock))))
(defun structed-release-local-lock ()
  (structed-send-command '((type . release_local_lock))))

(defun structed-edit-current-node ()
  "Put the current node into edit mode"
  (interactive)
  (let ((tree-index (structed-get-current-tree-index)))
    (setq structed-is-editing t)
    (structed-hold-local-lock)
    (structed-set-editing-block tree-index)
    ;; TODO: color the text
    ))

(defun structed-set-editing-block (tree-index)
  "Set the node's costraints for editing, and move point there, assumes point is on the line"
  (search-backward-regexp "^\\|\\n")
  (search-forward "\"")
  (let ((quote-block-start (- (point) 1)))
    (search-forward "\"\n")
    (let ((quote-block-end (- (point) 1))
          (inhibit-read-only t))
      (message "Add text property from %d to %d" quote-block-start quote-block-end)
      (add-text-properties quote-block-start quote-block-end 
                           '(face highlight))
      (add-text-properties quote-block-start (- quote-block-end 1)
                           '(read-only structed-editing-block))
      (goto-char (+ 1 quote-block-start)))))

