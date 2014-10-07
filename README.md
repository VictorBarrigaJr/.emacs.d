# README #

This repository contains a customized configuration file for:
~/.emacs.d/init.el


### Install ###

How to clone the project:

  REPO=~/dev/emacs.d
  mkdir -p $REPO
  git clone https://github.com/VictorBarrigaJr/.emacs.d.git 

To install:

  mkdir -p ~/.emacs.d
  ( cd ~/.emacs.d && ln -s $REPO/init.el . )
  /path/to/emacs

The idea is to keep the git repository out of the way in some place of your
own, then link it at the right place.

Please consult
http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html[the
Emacs manual regarding init files] to insure that +$REPO/init.el+ will
actually be loaded. 


### What's in there ###

Linked libraries, some selection of packages, settings to disable and enable 
window settings and theme, etc.  The visual improvements can be easily 
reversed or updated.


### Manual steps ###

Once the first startup is done, consider +M-x el-get-emacswiki-refresh+ so
that you're able to easily install any package from +emacswiki+. Just add 
the packages you want to try and need.  To do that, try +M-x
el-get-install+ then +TAB+, and you will have a list

In the future this very simple emacs setup will grow and use several files.


### Changing Preferences ###

If you want a different setup from the basic emacs here, the simplest way is 
to fork the project on github then use your own prefrences to add or delete
settings. 
