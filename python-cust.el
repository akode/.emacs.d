(autoload 'python-mode "python-mode" "Python mode." t)
(add-hook 'python-mode-hook 'linum-mode)
(eval-after-load "python-mode"
  '(progn
     (setq-default py-shell-name "ipython")
     (setq-default py-which-bufname "IPython")
))
;; pymacs
(autoload 'pymacs-apply "pyacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(setq py-load-pymacs-p t)

;; ropemacs
(autoload 'pymacs "pymacs" "Pymacs mode." t)
(pymacs-load "ropemacs" "rope-")
