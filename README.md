# My Emacs Configuration #

It uses `package.el` to install suplementary modes. Therefore it requires Emacs 24! As I'm mostly using Mac my configuration might contain some Mac specific modifications that don't work on other systems. I will check this soon.The current configuration works on Windows too.

All customizations are made in `init.el`. The `.emacs` file is just used for loading `init.el` and setting some variables. This is done by

```cl
(load-file "~/emacs.d/init.el")
```

Computer specific configurations are set in `.emacs`

```cl
(setq my-org-directories (list ""))
(setq org-capture-task-file "~/tasks.org")
(setq org-capture-call-file "~/calls.org")
(setq org-agenda-files (list ""))
```

## Loaded packages ##

* Yasnippets
* AucTeX
* OrgMode
* Smartparens
* Elpy
* Jinja2-mode
* Auto-complete
* Ido-mode
* Egg

