(require 'json)

(defvar struced-mode-hook nil)

(defvar structed-mode-keymap
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for the Structed Major Mode")

(defvar structed-buffer-name "*Structed*"
  "Buffer name to open structed instances in")

(defun structed ()
  "Open a new structed buffer"
  (interactive)
  (let ((existing-buffer (structed-find-buffer)))
    (if existing-buffer
        (pop-to-buffer existing-buffer)
      (progn (pop-to-buffer (generate-new-buffer structed-buffer-name))
             (structed-mode-init)))))

(defun structed-find-buffer ()
  "Look for an existing structed buffer and return it, else nil"
  (dolist (buf (buffer-list) buf)
    (if (with-current-buffer buf
          (and (eq major-mode 'structed-mode)
               (equal structed-buffer-name (buffer-name))))
        (return buf))))

(defun structed-mode ()
  "Major Mode for using Structed"
  (interactive)
  (structed-mode-init))

(defun structed-mode-init ()
  "Initializer for structed mode"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map structed-mode-keymap)
  (setq major-mode 'structed-mode)
  (setq major-name "Structed")
  (run-hooks 'structed-mode-hook)
  (structed-start-process))

(defun structed-draw-on-output-filter (process output)
  "Accepts the stdout of the structed python client process"
  (message "structed got: %s" output)
  (let ((buf (structed-find-buffer)))
    (if buf
        (with-current-buffer buf
          (structed-draw-tree output))
      (progn
        (message "Couldn't find Structed Buffer")
        (kill-process process)))))

(defvar structed-client-buffer-name "*structed-client*")

(defun structed-start-process ()
  "Starts the background python process"
  (set-process-filter (start-process-shell-command
                       "structed-client"
                       structed-client-buffer-name
                       "python"
                       "/Users/leighpauls/structed_client.py"
                       "2>/dev/null")
                      'structed-draw-on-output-filter))

(defun structed-draw-layer (num-dots json-tree)
  "Draw the subtree recursively"
  (let ((buffer-read-only nil))
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
        (buffer-read-only nil))
    (save-excursion
      (delete-region (point-min) (point-max))
      (structed-draw-layer 1 json-tree))
    (goto-char (min old-point (point-max)))))

(defun structed-append-root-node ()
  "Dummy function to append a child to the root node"
  (interactive)
  (process-send-string 
   structed-client-buffer-name
   (concat
    (json-encode '(:type "append" :tree_index []))
    "\n")))

(defun structed-get-line-depth (line)
  "Get how deep this line is"
  (let ((res (- (/ (string-match "\\\"" line) 2) 1)))
    (message "line: %s, val: %s" line res)
    res))

(defun remove-empties (strings)
  (let ((res (list)))
    (dolist (str strings)
      (unless (= 0 (length str))
        (setq res (append res (list str)))))
    res))
    

(defun structed-get-current-tree-index ()
  "Return the current index of the pointer"
  (interactive)
  (save-excursion
    (re-search-forward "\n")
    (structed-do-get-line-depth
     (remove-empties (split-string (buffer-substring
                                    (point-min)
                                    (point)) "\n")))))

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
