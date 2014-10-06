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
	 ggtags iedit minimap notify package popup smex smooth-scrolling
	 switch-window volatile-highlights yasnippet smartparens undo-tree 
         whitespace company-mode golden-ratio rainbow-mode)))

(el-get 'sync my:el-get-packages)

;;(require 'ansi-color) 

;; start Emacs with Bookmark
(setq inhibit-startup-message t) ;; no splash screen at startup
(setq inhibit-splash-screen t) 
(require 'bookmark) ;; sets bookmarks to files and locations to open later
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

;; Company is a text completion framework, to use on all buffers 
(add-hook 'after-init-hook 'global-company-mode)

;; occur key binding find all occurances of string
(global-set-key (kbd "C-c o") 'occur)

;; Interactively Do Things - minibuffer 
(require 'ido) 
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-enable-prefix nil)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(setq ido-max-prospects 10)
(setq ido-default-file-method 'selected-window)
(setq ido-auto-merge-work-directories-length -1)
(ido-mode 1)

;; Find File at Point - used to open a directory path 
(require 'ffap) 

;; (Di)rectory (Ed)itor - for file management
(require 'dired-x)

(require 'cl) ;; common list 

(require 'flymake)

;; Package: smartparens
(require 'smartparens-config)
(show-smartparens-global-mode +1)

(require 'smooth-scrolling)

;; Undo-tree - editing - view whole history of editing in a tree
(require 'undo-tree)
(global-undo-tree-mode)

(require 'uniquify)

;;Volatile-Highlights - edit tool - highlights changes to buffer
(require 'volatile-highlights)
(volatile-highlights-mode t) 

;; White Space package - visualize blanks
(require 'whitespace)
(global-set-key (kbd "C-c w") 'whitespace-mode)
;; whenever you create useless whitespace, the whitespace is highlighted
(add-hook 'prog-mode-hook 
          (lambda () 
            (interactive) (setq show-trailing-whitespace 1)))

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

;; rainbow-mode package sets background color to string matches
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

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
(setq ibuffer-use-other-window t) ;; display buffer in another window

;; Ctrl X-Ctrl S button dangerously close to Ctrl X-Ctrl C
(setq confirm-kill-emacs 'yes-or-no-p)


;; file settings
;; recent files
(require 'recentf)
(setq
 recentf-max-menu-items 30
 recentf-max-saved-items 5000
)
;; save all backups in one folder
(setq backup-directory-alist '(("."."~/.emacs.d/saves")))
;; group: Files 
(setq large-file-warning-threshold 100000000) ;; size in bytes
(setq 
 make-backup-files t        ; backup a file the first time it is saved
 backup-by-copying t     ; copy the current file into backup directory
 version-control t   ; version numbers for backup files
 delete-old-versions t   ; delete unnecessary versions
 kept-old-versions 6     ; oldest versions to keep (default: 2)
 kept-new-versions 9 ; newest versions to keep (default: 2)
 auto-save-default t ; auto-save every buffer that visits a file
 auto-save-timeout 20 ; idle time before auto-save (default: 30)
 auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
)


;; c cc-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(add-hook 'dired-mode-hook 'ggtags-mode)


;; add the bottom line to a set-up-programming.el to separate package  
;;(provide 'setup-programming)
;; GROUP: Programming -> Languages -> C

;; Available C styles: 
;; "gnu": The default style for GNU projects
;; "k&": Kernighan and Ritchie, the authors of C 
;; "bsd": BSD developers use
;; "whitesmith": Popularized by the examples that came with Whitesmiths C
;; "stroustrup": What Stroustrup, the author of C++ 
;; ''ellemetel":Popular C++ coding standards as defined Programming in C++
;; "linux": Linux developers use for kernel 
;; "python": Python developers use for extension 
;; "java": The default style for java-mode 
;; "user": define your own style
(setq c-default-style "gnu" ; set to "gnu"
      c-basic-offset 4)

;; GROUP: Programming -> Tools -> Gdb ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gdb-many-windows t        ; use gdb-many-windows by default
      gdb-show-main t)          ; Non-nil means display source file containing the main routine at startup

;; GROUP: Programming -> Tools -> Compilation ;;

;; Compilation from Emacs
(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

;; setup compilation-mode  
(require 'compile)
(setq compilation-ask-about-save nil          ; Just save before compiling
      compilation-always-kill t               ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error) ; Automatically scroll to first
(global-set-key (kbd "<f5>") 'compile)

;; GROUP: Programming -> Tools -> Makefile
;; takenn from prelude-c.el:48: https://github.com/bbatsov/prelude/blob/master/modules/prelude-c.el
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))



;; screen/window settings
;; navigate windows with M-<arros>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)
;;golden ratio package
(require 'golden-ratio)
(golden-ratio-mode)


;; notifies you once emacs is ready to use and the load time 
(defun dim:notify-startup-done ()
  " notify user that Emacs is now ready"
  (el-get-notify
   "Emacs is ready."
   (format "The init sequence took %g seconds."
	      (float-time (time-subtract after-init-time before-init-time)))))

(add-hook 'after-init-hook 'dim:notify-startup-done)
