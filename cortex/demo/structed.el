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
  (setq debug-on-error t)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map structed-mode-keymap)
  (setq major-mode 'structed-mode)
  (setq major-name "Structed")
  (add-hook 'post-self-insert-hook 'structed-post-insert-handler nil t)
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

(defun structed-start-process ()
  "Starts the background python process"
  (set-process-filter (start-process-shell-command
                       "structed-client"
                       "*structed-client*"
                       "python"
                       "/Users/leighpauls/structed_client.py"
                       "2>/dev/null")
                      'structed-draw-on-output-filter))

(defun structed-draw-layer (num-dots json-tree)
  "Draw the subtree recursively"
  (let ((buffer-read-only nil))
    (dotimes (n num-dots nil)
      (insert "* "))
    (let ((value (cdr (assoc 'value json-tree))))
      (dotimes (i (length value) nil)
        (insert (aref value i))))
    (insert "\n")
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
