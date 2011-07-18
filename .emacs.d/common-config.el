
;-----------------------;
; Emacs customization   ;
;-----------------------;
;(setq debug-on-error t)
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/modes")
(add-to-list 'load-path "~/.emacs.d/utils")


; prevent dabbrev from replacing case
(setq dabbrev-case-replace nil)

;-----------------------;
; Misc configuration    ;
;-----------------------;
(setq x-select-enable-clipboard t)
(scroll-bar-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq auto-save-list-file-prefix nil)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 8)
(setq default-tab-indent 4)
(setq make-backup-files nil)
(setq max-mini-window-height 1)
(setq transient-mark-mode t)
(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0)
  )
(add-to-list 'exec-path "/usr/local/bin")

;-----------------------------------------;
; Assume new files are always modified    ;
;-----------------------------------------;
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

;-----------------------;
; Keyboard shortcuts    ;
;-----------------------;
(require 'keyboard-shortcuts)

; ido-mode
(require 'ido)
(ido-mode t)
(setq ring-bell-function 'ignore)

(require 'ido-recentf-open)

;----------------------------;
; Python                     ;
;----------------------------;
(load "~/.emacs.d/python/python-config.el")

;
; recentf
;
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 1000)
(recentf-mode 1)
(add-hook 'find-file-hooks 'recentf-save-list)

;----------------------------;
; anything.el                ;
;----------------------------;
(require 'anything)
(require 'anything-config)
(global-set-key [(control x) (a)] 'anything)
(setq fit-frame-inhibit-fitting-flag t)
(setq history-length 500)
(setq anything-sources
       (list
             anything-c-source-buffers
             anything-c-source-recentf
             anything-c-source-file-name-history
             anything-c-source-occur
             anything-c-source-fixme
             anything-c-source-ctags
             anything-c-source-kill-ring
;             anything-c-source-locate
             anything-c-source-emacs-commands
             ;anything-c-source-mac-spotlight
             ; aweseome - find todo notes
;             anything-c-source-buffer-not-found

;             anything-c-source-info-pages
;             anything-c-source-info-elisp
;             anything-c-source-man-pages
             ))

;----------------------------;
; Changing active buffer     ;
;----------------------------;
(require 'windmove)
(global-set-key [(control x) up] 'windmove-up)
(global-set-key [(meta up)] 'windmove-up)
(global-set-key [(control x) down] 'windmove-down)
(global-set-key [(meta down)] 'windmove-down)
(global-set-key [(control x) left] 'windmove-left)
(global-set-key [(meta left)] 'windmove-left)
(global-set-key [(control x) right] 'windmove-right)
(global-set-key [(meta right)] 'windmove-right)

;-----------------------------;
;Template support             ;
;-----------------------------;
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")
(setq yas/indent-line 'fixed)

;-------------------------------------------
; auto-complete
;-------------------------------------------
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)

;-------------------------------------------
; magit
;-------------------------------------------
(add-to-list 'load-path "~/.emacs.d/magit")
(require 'magit)

;----------------------------;
; IRC                        ;
;----------------------------;
(require 'rcirc)

;----------------------------;
; Swapping windows           ;
;----------------------------;
(require 'swap-windows)
(global-set-key [(control x) (control t)] 'swap-windows)

;------------------------------------------------------------------------------;
; linum-mode                                                                   ;
;------------------------------------------------------------------------------;
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'javascript-mode-hook 'linum-mode)
(add-hook 'js-mode-hook 'linum-mode)
(add-hook 'cc-mode-hook 'linum-mode)

(add-hook 'find-file-hooks 'hl-line-mode)

;------------------------------------------------------------------------------;
; smex
;------------------------------------------------------------------------------;
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands) 

;------------------------------------------------------------------------------;
; C/C++
;------------------------------------------------------------------------------;
(setq compilation-window-height 8)
(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, press C-x ` to visit")

          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))
    

;------------------------------------------------------------------------------;
; Customization                                                                ;
;------------------------------------------------------------------------------;
(require 'customization)