# My Emacs Configuration #

It uses `package.el` to install suplementary modes. Therefore it requires Emacs 24! As I'm mostly using Mac my configuration might contain some Mac specific modifications that don't work on other systems. I will check this soon.

All customizations are made in `init.el`. The `.emacs` file is just used for loading `init.el` and setting some variables. This is done by

```cl
(load-file "~/emacs.d/init.el")
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
* Calfw
* ESS
* Matlab-emacs (not in package manager)
