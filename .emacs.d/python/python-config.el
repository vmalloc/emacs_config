;; Misc. Python settings
(add-to-list 'load-path "~/.emacs.d/python")
;----------------------------;
; python-mode                ;
;----------------------------;
(require 'python-mode)

;;----------------------------;
;; ropemacs                   ;
;;----------------------------;
;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport 't)
;(setq ropemacs-autoimport-modules '("os" "shutil" "time" "re"))
;(setq ropemacs-enable-shortcuts 'nil)

;(defun bind-ropemacs-shortcuts ()
;  (local-set-key [(control ?x) ?p ?c] 'rope-code-assist)
;  )

;(add-hook 'python-mode-hook 'bind-ropemacs-shortcuts)


;----------------------;
; automatic super()    ;
;----------------------;
(defun py-insert-super ()
  (interactive)
  (let (methodname classname)
    (save-excursion
      (or (py-go-up-tree-to-keyword "def")
          (error "Enclosing def not found"))
      (or (looking-at "[ \t]*def[ \t]+\\([a-zA-Z0-9_]+\\)")
          (error "Can't determine method name"))
      (setq methodname (match-string 1))
      (or (py-go-up-tree-to-keyword "class")
          (error "Enclosing class not found"))
      (or (looking-at "[ \t]*class[ \t]+\\([a-zA-Z0-9_]+\\)")
          (error "Can't determine class name"))
      (setq classname (match-string 1)))
    (insert (format "super(%s, self).%s()" classname methodname))
    (backward-char)))

;; Add a hook to bind a key to this function for Python buffers
(defun bind-super-key ()
  (local-set-key [(control ?x) ?p ?s] 'py-insert-super))

(add-hook 'python-mode-hook 'bind-super-key)

;------------------------------------------------------------------------------;
; ipython-mode                                                                 ;
;------------------------------------------------------------------------------;
(require 'ipython)

;------------------------------------------------------------------------------;
; highlight trailing whitespaces                                               ;
;------------------------------------------------------------------------------;
(add-hook 'python-mode-hook
   '(lambda()
      (setq show-trailing-whitespace t)
    )
)

;----------------------------;
; pylint                     ;
;----------------------------;
(setq pycodechecker "pyflakes")
(when (load "flymake" t)
  (global-set-key [(f1)]                   'flymake-display-err-menu-for-current-line)
  (global-set-key [(f2)]                   'flymake-goto-prev-error)
  (global-set-key [(f3)]                   'flymake-goto-next-error)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

;(add-hook 'python-mode-hook 'flymake-mode)

;-----------------------;
; Additional Modes      ;
;-----------------------;
(load "~/.emacs.d/python/doctest-mode.el")
(load "~/.emacs.d/python/django-mode.el")

;-----------------------;
; pdb                   ;
;-----------------------;
(setq pdb-command "python -m pdb")
(setq pdb-path 'pdb-command)
(setq gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
   "Provide a better default command line when called interactively."
   (interactive
    (list (gud-query-cmdline pdb-path
	 		    (file-name-nondirectory buffer-file-name)))))
