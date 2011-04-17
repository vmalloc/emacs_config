(load-file "~/.emacs.d/common-config.el")

(if (not (window-system))
    (load-file "~/.emacs.d/terminal-config.el")
    (load-file "~/.emacs.d/window-config.el")
    )