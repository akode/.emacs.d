;; two variables are defined in .emacs
;; my-gcal-priv : address of the my google mail calendar
;; my-org-directories : first of the list is the main directory

(server-start)

;; crossplatform helpers
(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

(defun system-is-windows ()
  (interactive)
  (string-equal system-type "windows-nt"))

;; Load paths
(let ((default-directory "~/.emacs.d/config/"))
  (normal-top-level-add-to-load-path '("." "../packages/matlab-emacs" "../packages/ensime_2.10.0-0.9.8.9/elisp")))
;; if these packages are not present just comment matlab-mode out

;; Packages
(defvar sys-packages
  '(auctex auto-complete smartparens yasnippet markdown-mode less-css-mode coffee-mode egg elpy scala-mode2)
  "List of packages installed via packages system from elpa and melpa.")

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			  ("melpa" . "http://melpa.milkbox.net/packages/")
			  ("SC"   . "http://joseito.republika.pl/sunrise-commander/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (p sys-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;
;; visual modifications
(setq inhibit-startup-message t)
(global-visual-line-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;(set-cursor-color 'red)
(show-paren-mode t)

;; sound
(setq ring-bell-function #'ignore)

;;;;;;;;;;;;;;;;;;;;
;; keyboard modifications
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-x f") 'find-file-at-point)

;;;;;;;;;;;;;;;;;;;;
;; Timestamp (add "Time-stamp: <>" in one of the first 8 lines of the file)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-pattern nil)

;;;;;;;;;;;;;;;;;;;;
;; Spellcheck
(add-to-list 'ispell-tex-skip-alists '(("\\\\gls" ispell-tex-arg-end)
				       ("\\\\glspl" ispell-tex-arg-end)
				       ("\\\\citep" ispell-tex-arg-end)
				       ("\\\\autoref" ispell-tex-arg-end)))
(add-hook 'rst-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;
;; Egg - Emacs got GIT
(delete 'Git vc-handled-backends)
(require 'egg)

;;;;;;;;;;;;;;;;;;;;
;; SASS css mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))

;;;;;;;;;;;;;;;;;;;;
;; Coffee script mode
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

;;;;;;;;;;;;;;;;;;;;
;; Web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;
;; Ensime/Scala
(autoload 'ensime "ensime" "Ensime Scala mode." t)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;;;;;;;;;;;;;;;;;;;
;; Matlab-mode
;(require 'matlab-load)

;;;;;;;;;;;;;;;;;;;;
;; org-pandoc
;(require 'ox-pandoc)

;;;;;;;;;;;;;;;;;;;;
;; Python
(elpy-enable)
(elpy-use-ipython)

;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
(autoload 'markdown-mode "markdown-mode" "Markdown mode." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.html.org\\'" . org-mode))

;;;;;;;;;;;;;;;;;;;;
;; ESS
;(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;
;; Autocomplete
(require 'auto-complete)
(add-hook 'python-mode-hook 'auto-complete-mode)

;;;;;;;;;;;;;;;;;;;;
;; Smartparens
(smartparens-global-mode t)
(eval-after-load "smartparens-mode"
  '(progn
     (sp-local-pair 'latex-mode "\\left(" "\\right)")
))

;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
;; yasnippet mode is loaded only for specific modes
(eval-after-load "yasnippet"
  '(progn
     (setq yas/root-directory (file-expand-wildcards "~/.emacs.d/elpa/yasnippet*/snippets"))
     (mapc 'yas/load-directory yas/root-directory)
     (setf yas/indent-line nil)
))

(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
(add-hook 'rst-mode-hook 'yas-minor-mode)
(add-hook 'web-mode-hook 'yas-minor-mode)

;;;;;;;;;;;;;;;;;;;;
;; IDO
(setq ide-enable-flex-matching t)
(setq ido-everywhere t)
(setq org-completion-use-ido t)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;
;; OrgMode
(autoload 'org-latex "org-latex" t)
(setq org-directory (car my-org-directories))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq diary-file (concat org-directory "/.diary"))
(setq org-agenda-files my-org-directories)

(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))

(setq org-modules (quote (org-habit
			  org-bibtex
			  org-mac-link-grabber)))

(load "org-cust")

;;;;;;;;;;;;;;;;;;;;
;; AucTeX
(setq TeX-PDF-mode t)
(setq TeX-auto-global "~/emacs-files/auto")
(load "auctex-cust")

;;;;;;;;;;;;;;;;;;;;
;; toggle fullscreen function (tested only on Mac)
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;;;;;;;;;;;;;;;;;;;;
;; byte compile elisp files on save, but only if a byte compiled version already exists
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)
