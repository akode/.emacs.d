;; two variables are defined in .emacs
;; my-gcal-priv : address of the my google mail calendar
;; my-org-directory : path to my org files

(server-start)

;; Load paths
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("." "packages/matlab-emacs")))
;; if these packages are not present just comment matlab-mode out

;; Packages
(defvar sys-packages
  '(auctex auto-complete less-css-mode smartparens yasnippet markdown-mode calfw ess jinja2-mode egg elpy)
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
(set-cursor-color 'red)
(show-paren-mode t)

;; sound
(setq ring-bell-function #'ignore)

;;;;;;;;;;;;;;;;;;;;
;; keyboard modifications
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
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
;; Matlab-mode
(require 'matlab-load)

;;;;;;;;;;;;;;;;;;;;
;; Python
(elpy-enable)
(elpy-use-ipython)

;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
(autoload 'markdown-mode "markdown-mode" "Markdown mode." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;
;; ESS
(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;
;; Jinja2
(autoload 'jinja2-mode "jinja2-mode" "Jinja2 mode." t)

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
(add-hook 'html-mode-hook 'yas-minor-mode)

;;;;;;;;;;;;;;;;;;;;
;; IDO
(setq ide-enable-flex-matching t)
(setq ido-everywhere t)
(setq org-completion-use-ido t)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;
;; Calendar/calfw
(require 'calfw)
(require 'calfw-org)
(require 'calfw-ical)
(defun open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    (cfw:ical-create-source "gcal" my-gcal-priv "IndianRed")
    )))

(load "calendar-cust")

;;;;;;;;;;;;;;;;;;;;
;; OrgMode
(autoload 'org-latex "org-latex" t)
(setq org-directory my-org-directory)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq diary-file (concat org-directory "/.diary"))
(setq org-agenda-files (list my-org-directory))

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
