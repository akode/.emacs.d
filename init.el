;; two variables are defined in .emacs
;; my-gcal-priv : address of the my google mail calendar
;; my-org-directory : path to my org files

(server-start)

;; Load paths
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("." "packages/python-mode" "packages/pymacs" "packages/ropemacs" "packages/matlab-emacs")))
;; if these packages are not present just comment Python and matlab-mode out

;; Packages
(defvar sys-packages
  '(auctex auto-complete less-css-mode smartparens yasnippet markdown-mode calfw ess jinja2-mode)
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
(global-set-key (kbd "\C-x g") 'magit-status)

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
(add-hook 'latex-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;
;; Matlab-mode
(require 'matlab-load)

;;;;;;;;;;;;;;;;;;;;
;; Python
(load "python-cust.el")

;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
(autoload 'markdown-mode "markdown-mode" "Markdown mode." t)

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
     (setq yas/root-directory '("~/.emacs.d/elpa/yasnippet-20130218.2229/snippets"))
     (mapc 'yas/load-directory yas/root-directory)
     (setf yas/indent-line nil)
))

(add-hook 'latex-mode-hook 'yas-minor-mode)
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

(load "calendar-cust.el")

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

(add-hook 'org-mode-hook (lambda ()
			   (define-key org-mode-map (kbd "C-c g") 'omlg-grab-link)))
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-log-done 'time)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/@)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "CALL(p)"))))

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task to STARTED when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist (quote history))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks."
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      "STARTED"))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;;;;;;;;;;;;;;;;;;;;
;; AucTeX
(setq TeX-PDF-mode t)
(setq LaTeX-command "latex --synctex=1")
(setq TeX-auto-global "~/emacs-files/auto")
(when (eq system-type 'darwin)
  (setq TeX-view-program-list (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))))
  (setq TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Skim") (output-html "xdg-open")))))

(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq-default TeX-master nil)

;; AucTeX with Biber
(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber." 
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-Biber-sentinel (process name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ;; Check whether Biber reports any warnings or errors.
   ((re-search-backward (concat
			 "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
			 "\\(warnings?\\|error messages?\\))") nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "Biber finished with %s %s. "
		     "Type `%s' to display output.")
	     (match-string 1) (match-string 2)
	     (substitute-command-keys
	      "\\\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "Biber finished successfully. "
		     "Run LaTeX again to get citations right."))))
  (setq TeX-command-next TeX-command-default))
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))
  )
