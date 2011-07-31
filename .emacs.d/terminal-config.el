(menu-bar-mode nil)

;------------------------------------------------------------------------------;
; Solve the Shift+Up issue for terminal emacs                                  ;
;------------------------------------------------------------------------------;
(define-key input-decode-map "\e[1;2A" [S-up])

;------------------------------------------------------------------------------;
; Control-backspace hack for Mac terminals. You need to make your term send    ;
; this sequence in order for this to work                                      ;
;------------------------------------------------------------------------------;
(define-key input-decode-map "\e?" [C-backspace])