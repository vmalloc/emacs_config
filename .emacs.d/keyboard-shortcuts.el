(global-set-key [(control z)]            'undo)
(global-set-key [(control =)]            'redo)
(global-set-key [(shift delete)]         'kill-region)
(global-set-key [(control insert)]       'copy-region-as-kill)
(global-set-key [(shift insert)]         'yank)
(global-set-key [(f3)]                   'mode-line-other-buffer)
(global-set-key [(meta g)]               'goto-line)
(global-set-key [(f4)]                   'next-error)
(global-set-key [(shift f4)]             'previous-error)
(global-set-key [(control x) (meta r)]  'revert-buffer)

(global-set-key [(control right-arrow)]  'end-of-line)
(global-set-key [(control left-arrow)]   'beginning-of-line)
(global-set-key [(control home)]         'beginning-of-buffer)
(global-set-key [(control end)]          'end-of-buffer)
(global-set-key [(control tab)]          'other-window)
(global-set-key [(meta delete)]          'kill-word)
(global-set-key [(backspace)]            'delete-backward-char)
(delete-selection-mode 1)
(global-set-key [(kp-delete)]            'delete-char)
(global-set-key [(end)]                  'end-of-line)
(global-set-key [(home)]                 'beginning-of-line)
(defun scroll-one-down  () (interactive) (scroll-up    1))
(defun scroll-one-up    () (interactive) (scroll-down  1))
(defun scroll-one-left  () (interactive) (scroll-right 1))
(defun scroll-one-right () (interactive) (scroll-left  1))

(global-set-key [(control right)]         'forward-word)
(global-set-key [(control left)]          'backward-word)

(defun yy-other-window nil
  "Go to next window, but never one holding a minibuffer"
  (interactive)
  (other-window 1)
  (while (minibufferp (current-buffer)) (other-window 1)))
(global-set-key [(control x) ?o] 'yy-other-window)

(defun close-unmodified-buffers nil
  "Close all unmodified buffers"
  (interactive)
  (loop for buffer being the buffers
        when (not (equalp buffer (current-buffer)))
        when (not (buffer-modified-p buffer))
        collect (kill-buffer buffer)))
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key [(shift f1)]             'kill-current-buffer)

(global-set-key [(f12)] 'delete-trailing-whitespace)
(add-hook 'python-mode-hook
   '(lambda()
      (setq show-trailing-whitespace t)
    )
)
