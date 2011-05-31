;--------------------------;
; Color theme & appearance ;
;--------------------------;
(load "~/.emacs.d/color-theme.el")
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)
(if (boundp 'set-frame-parameter)
    (set-frame-parameter (selected-frame) 'alpha 90)
)

;-----------------------;
; Make searching fun    ;
;-----------------------;

(setq isearch-lazy-highlight-initial-delay 0)
(setq isearch-lazy-highlight-interval      0)
(define-key isearch-mode-map [backspace] 'isearch-del-char)
(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)


;----------------------;
; Goodies from cygwin  ;
;----------------------;

(global-set-key [13] 'newline-and-indent)
(add-hook 'c-mode-hook
   '(lambda()
	(local-set-key [13] 'newline-and-indent)
	(setq c-basic-offset 4)
	(c-set-offset 'substatement-open 0)
    )
)

(add-hook 'c++-mode-hook
   '(lambda()
	(local-set-key [13] 'newline-and-indent)
	(setq c-basic-offset 4)
	(c-set-offset 'substatement-open 0)
    )
)

(setq initial-frame-alist '((left . 50) (width . 170) (height . 40)  ))

;----------------------------;
; Uniquify                   ;
;----------------------------;
(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;----------------------------;
; Title                      ;
;----------------------------;
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;----------------------------;
; vc-git                     ;
;----------------------------;
 (require 'git)
 (autoload 'git-blame-mode "git-blame"
           "Minor mode for incremental blame for Git." t)
 (defun git-blame-really-identify ()
   "Since git-blame-identify for some reason hides the description, this is a version that actually works"
   (interactive)
   (git-blame-identify)
   (message (nth 4 git-blame-last-identification)))
 (when (load "git-blame" t)
   (global-set-key [(f1)] 'git-blame-really-identify))

;----------------------------;
; gpg                        ;
;----------------------------;
(require 'epa-file)
(epa-file-enable)

;----------------------------;
; org-mode                   ;
;----------------------------;
(setq my-org-dir "~/workspace/personal/documents/orgs")
(setq my-work-org-file (concat my-org-dir "/work.org"))
(setq my-personal-org-file (concat my-org-dir "/personal.org"))
(global-set-key [(f5)] '(lambda () (interactive) (find-file my-work-org-file)))
(global-set-key [(shift f5)] '(lambda () (interactive) (find-file my-personal-org-file)))

(setq org-agenda-files (list my-work-org-file my-personal-org-file))
(setq org-replace-disputed-keys t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-deadline-warning-days 5)

;----------------------------;
; tramp                      ;
;----------------------------;
(require 'tramp)

;----------------------------;
; Passwords                  ;
;----------------------------;
(setq my-passwords-file (concat my-org-dir "/passwords.gpg"))
(defun open-my-passwords-file ()
  (interactive)
  (find-file my-passwords-file))
(global-set-key [(f6)] 'open-my-passwords-file)

;----------------------------;
; fun                        ;
;----------------------------;
(require 'tetris)
(define-key tetris-mode-map [down] 'tetris-move-bottom)

;----------------------------;
; haskell-mode               ;
;----------------------------;
(add-to-list 'load-path "~/.emacs.d/modes/haskell-mode")
(require 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(autoload 'turn-on-haskell-ghci "haskell-ghci"
  "Turn on interaction with a GHCi interpreter." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

;----------------------------;
; artist.el                  ;
;----------------------------;
(require 'artist)

;----------------------------;
; query-replace              ;
;----------------------------;
(defun better-query-replace-regexp (regexp to-string)
  (interactive "sRegexp: \nsString: ")
  (save-excursion
    (beginning-of-buffer)
    (query-replace-regexp regexp to-string)))
(global-set-key [(meta ?%)] 'better-query-replace-regexp)

;-----------------------;
; Color settings        ;
;-----------------------;
(ansi-color-for-comint-mode-on)

;----------------------;
; Writing C/C++        ;
;----------------------;

; switch header/imp
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;-------------------------;
; save position in files  ;
;-------------------------;
(setq save-place-file "~/.emacs.d/saveplace") 
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "apple" :family "Monaco")))))

;-------------------------;
; ibuffers                ;
;-------------------------;
(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
  (quote (("default"      
            ("Programming" ;; prog stuff not already in MyProjectX
              (or
                (mode . c-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . emacs-lisp-mode)
                ;; etc
                ))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;--------------------------------------------
; Web development
;--------------------------------------------
(add-to-list 'load-path "~/.emacs.d/modes/javascript-mode")
(require 'javascript-mode)
(load "~/.emacs.d/modes/nxhtml-mode/autostart.el")
(require 'mumamo-customization)
(add-to-list 'load-path "~/.emacs.d/utils/jquery-doc")
(require 'jquery-doc)

;--------------------------------------------
; browse kill ring
;--------------------------------------------
( when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))
(global-set-key (kbd "C-c C-y") '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))


(defun maximize-frame () 
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))
(maximize-frame)

;-------------------------------------------
; autopair
;-------------------------------------------
(require 'autopair)
(autopair-global-mode)
; compatibility with delete-selection-mode
(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

;---------------------------------------------
; multiterm
;---------------------------------------------
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook #' (lambda () (setq autopair-dont-activate t)))

;--------------------------------------------
; switch to header files in C/C++
;--------------------------------------------
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  [(f7)] 'ff-find-other-file)))

;--------------------------------------------
; Spell checking
;--------------------------------------------
(setq ispell-program-name "aspell")

;--------------------------------------------
; SCons building
;--------------------------------------------
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(global-set-key [(f9)] '(lambda () (interactive) (compile "scons" t)))

(defun process-error-filename (filename)
  (let ((case-fold-search t))
    (setq f (replace-regexp-in-string
             "[Ss]?[Bb]uild[\\/].*\\(final\\|dbg\\)[^\\/]*[\\/]" "" filename))
    (cond ((file-exists-p f)
           f)
          (t filename))))

(setq compilation-parse-errors-filename-function 'process-error-filename)

;--------------------------------------------
; nose and coverage
;--------------------------------------------
(require 'nose)
(setq nose-use-verbose nil)
(require 'python-coverage)

(global-set-key [(f10)] '(lambda () (interactive) (nosetests-all)))
(global-set-key [(f11)] 'python-coverage-buffer)


;--------------------------------------------
; drag stuff
;--------------------------------------------
(require 'drag-stuff)
(drag-stuff-global-mode t)
(define-key drag-stuff-mode-map (kbd "<C-M-up>") 'drag-stuff-up)
(define-key drag-stuff-mode-map (kbd "ESC <up>") 'drag-stuff-up)
(define-key drag-stuff-mode-map (kbd "<C-M-down>") 'drag-stuff-down)
(define-key drag-stuff-mode-map (kbd "ESC <down>") 'drag-stuff-down)
(define-key drag-stuff-mode-map (kbd "<C-M-right>") 'drag-stuff-right)
(define-key drag-stuff-mode-map (kbd "<C-M-left>") 'drag-stuff-left)
