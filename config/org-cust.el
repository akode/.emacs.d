(add-hook 'org-mode-hook (lambda ()
			   (define-key org-mode-map (kbd "C-c g") 'omlg-grab-link)))
(add-hook 'message-mode-hook 'orgtbl-mode 'append)

;; org-zotxt settings
(add-hook 'org-mode-hoook (lambda () (org-zotxt-mode 1)))
(add-hook 'org-mode-hook (lambda ()
			   (define-key org-mode-map (kbd "C-c i") (lambda () (interactive) (org-zotxt-insert-reference-link '(4))))))
(eval-after-load "zotxt"
  '(setq zotxt-default-bibliography-style "mkbehr-short"))


(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-log-done 'time)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/@)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "CALL(p)"))))

(setq org-capture-templates
      '(("q" "Todo" entry (file+headline org-capture-task-file "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("c" "Call" entry (file+datetree org-capture-call-file)
	 "* %? on %U\n  %i\n  %a" :clock-in t :clock-resume t)))

(setq user-full-name "Dr. Andreas Kodewitz")
(setq user-mail-address "")


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
