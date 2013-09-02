
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
