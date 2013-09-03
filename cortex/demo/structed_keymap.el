
(defun structed-define-non-edit-move-key (map key normal-function move-function)
  "Adds the key for non-editing, non-moving scenarios"
  (define-key map key `(lambda ()
                         (interactive)
                         (setq debug-on-error t)
                         (cond
                          (structed-is-moving 
                           (when (functionp ',move-function)
                             (,move-function)))
                          ((not structed-is-editing)
                           (when (functionp ',normal-function)
                             (,normal-function)))
                          (t
                           (insert ,key))))))

(defun structed-mode-make-keymap ()
  "Make the keymap for the Structed Major Mode"
  (let ((map (make-keymap)))
    (structed-define-non-edit-move-key map "a" 'structed-append-current-node 'structed-mode-move-insert-after)
    (structed-define-non-edit-move-key map "r" 'structed-remove-current-node nil)
    (structed-define-non-edit-move-key map "e" 'structed-edit-current-node nil)
    (structed-define-non-edit-move-key map "m" 'structed-move-current-node nil)
    (structed-define-non-edit-move-key map "b" nil 'structed-mode-move-insert-before)
    (structed-define-non-edit-move-key map "q" nil 'structed-mode-move-quit)
    (structed-define-non-edit-move-key map "p" 'structed-show-cur-pos nil)
    (define-key map (kbd "C-m") 'next-line)
    map))
