;; 
;; Copyright (C) 2014 Victor Barriga
;;
;; Author Victor Barriga <victorbarriga@live.com>
;; Created 2014-10-3
;;
;; Emacs configuration file init.el 
;; 
;; Licence: GNU GPL v3.0, http://www.gnu.org/licenses/gpl-3.0.txt
;;
;; This file is NOT part of GNU Emacs.

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;;adds melpa package library
(when (>= emacs-major-version 24)
  (require 'package)
 (package-initialize)
 (add-to-list 'package-archives 
	       '("melpa" . "http://melpa.milbox.net/packages/") t))

;;adds marmalade package library
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives 
	       '("marmalade" . "http://marmalade-repo.org/packages/") t))

;;adds el-get package library and recipes 
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously 
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
 
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-verbose t)

;; personal macros recipes
(setq el-get-sources
      '((:name buffer-move
	       :after (progn
			(global-set-key (kbd "<C-S-up>") 'buf-move-up)
			(global-set-key (kbd "<C-S-down>") 'buf-move-down)
			(global-set-key (kbd "<C-S-left>") 'buf-move-left)
			(global-set-key (kbd "<C-S-right>") 'buf-move-right)))

	(:name smex
	       :after (progn
			(setq smex-save-file "~/.emacs.d/.smex-items")
			(global-set-key (kbd "M-x") 'smex)
			(global-set-key (kbd "M-X") 'smex-major-mode-commands)))

	(:name goto-last-change
	       :after (progn
			(global-set-key (kbd "C-x C-/") 'goto-last-change)))
        (:name magit
	       :after (progn
			(global-set-key (kbd "C-x C-z") 'magit-status)))
	(:name iedit
	       :after (progn
			(global-set-key (kbd "C-c ;") 'iedit-mode)))))

;; my el-get packages
(setq my:el-get-packages
      (append
       '(auto-complete auto-complete-c-headers buffer-move color-theme el-get
	 emacs-goodies-el escreen fill-column-indicator fuzzy flymake gnus 	 
	 ggtags iedit minimap notify package popup smex switch-window 
         yasnippet)))

(el-get 'sync my:el-get-packages)

;;(require 'ansi-color) 

;; start Emacs with Bookmark
(setq inhibit-startup-message t) ;; no splash screen at startup
(setq inhibit-splash-screen t) 
(require 'bookmark) ;; sets bookmarks to files and locations to open later
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
;; occur key binding find all occurances of string
(global-set-key (kbd "C-c o") 'occur)

;; Interactively Do Things - minibuffer 
(require 'ido) 
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Find File at Point - used to open a directory path 
(require 'ffap) 

;; (Di)rectory (Ed)itor - for file management
(require 'dired-x)

(require 'cl) ;; common list 
(require 'compile)
(require 'recentf)
(require 'smooth-scrolling)
(require 'uniquify)
(require 'whitespace)
(require 'magit)

(ac-config-default) ;; auto-complete global
(yas-global-mode 1) ;; yasnippet global

;; visual settings
(menu-bar-mode -1) ;; remove menu toolbar from window
(line-number-mode 1) ;; line numbers in mode line
(column-number-mode 1) ;; column numbers in mode line
(setq column-number-mode t)
(global-linum-mode 1) ;; add line numbers to the left
(global-hl-line-mode) ;; highlight current line

;; color theme
(setq color-theme-is-global t) 
(color-theme-xemacs)

(custom-set-faces ;;custom settings  
 '(flymake-errline ((((class color) (background light)) (:background "Red"))))
 '(highlight ((((class color) (min-colors 8)) 
	       (:background "white" :foreground "magenta"))))
 '(linum ((t (:foreground "black" :weight bold))))
 '(vertical-border ((t nil))))

;; No tabs
(setq-default indent-tabs-mode nil)

;; add fill column
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-color "darkblue")
(setq fci-rule-column 80)

(normal-erase-is-backspace-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

;; replace list-buffers with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; navigate windows with M-<arros>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;;; save all backups in one folder
(setq backup-directory-alist '(("."."~/.emacs.d/saves")))

;; Ctrl X-Ctrl S button dangerously close to Ctrl X-Ctrl C
(setq confirm-kill-emacs 'yes-or-no-p)

;; c cc-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(add-hook 'dired-mode-hook 'ggtags-mode)


;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; notifies you once emacs is ready to use and the load time 
(defun dim:notify-startup-done ()
  " notify user that Emacs is now ready"
  (el-get-notify
   "Emacs is ready."
   (format "The init sequence took %g seconds."
	      (float-time (time-subtract after-init-time before-init-time)))))

(add-hook 'after-init-hook 'dim:notify-startup-done)
