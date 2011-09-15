
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

;-------------------------------------------
; magit
;-------------------------------------------
(add-to-list 'load-path "~/.emacs.d/modes/magit")
(require 'magit)

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
(run-with-timer 0 (* 5 60) 'recentf-save-list)

;----------------------------;
; anything.el                ;
;----------------------------;
(add-to-list 'load-path "~/.emacs.d/modes/anything")
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
             anything-c-source-kill-ring
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
(setq linum-format "%3d")
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'javascript-mode-hook 'linum-mode)
(add-hook 'js-mode-hook 'linum-mode)
(add-hook 'cc-mode-hook 'linum-mode)

; line highlighting mode
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
        (if (not (string-match "grep" (buffer-name buf)))
            (if (string-match "exited abnormally" str)

                ;;there were errors
                (message "compilation errors, press C-x ` to visit")
              
              ;;no errors, make the compilation window go away in 0.5 seconds
              (run-at-time 0.5 nil 'delete-windows-on buf)
              (message "NO COMPILATION ERRORS!")))))
(require 'xcscope)


;------------------------------------------------------------------------------;
; ace-jump-mode (https://github.com/winterTTr/ace-jump-mode)                   ;
;------------------------------------------------------------------------------;
(require 'ace-jump-mode)
(define-key global-map (kbd "C-x j") 'ace-jump-char-mode)
(setq ace-jump-mode-case-sensitive-search nil)

;------------------------------------------------------------------------------;
; Customization                                                                ;
;------------------------------------------------------------------------------;
(require 'customization)

(if (not (window-system))
    (load-file "~/.emacs.d/terminal-config.el")
    (load-file "~/.emacs.d/window-config.el")
    )

;------------------------------------------------------------------------------;
; Underlining                                                                  ;
;------------------------------------------------------------------------------;
(defun underline-current-line (underline_char) (interactive "sChar: ")
  (let (
        (i 0)
        )
    (save-excursion
      (setq length (- (line-end-position) (line-beginning-position)))
      (forward-line 1)
      (dotimes (i length)
        (insert (if underline_char underline_char "-"))
               )
    )
  )
)

;------------------------------------------------------------------------------;
; Dektop mode                                                                  ;
;------------------------------------------------------------------------------;
(desktop-save-mode 1)
(desktop-load-default)

; automatically save
(defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

; allow loading from 'locked' desktop files
(setq desktop-load-locked-desktop t)

; load previous
(desktop-read)
(add-hook 'kill-emacs-hook
          '(lambda ()
             (desktop-truncate search-ring 3)
             (desktop-truncate regexp-search-ring 3)))

; revive.el - used to restore frame configuration
(require 'revive)
(resume)
(add-hook 'kill-emacs-hook 'save-current-configuration)
