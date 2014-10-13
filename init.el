;;
;; Copyright (C) 2014 Victor Barriga
;;
;; Author Victor Barriga <victorbarriga@live.com>
;; Created 2014-10-3
;;
;; Emacs configuration file init.el
;;
;; License: GNU GPL v3.0, http://www.gnu.org/licenses/gpl-3.0.txt
;;
;; This file is NOT part of GNU Emacs.


;;--------------------------------------------------------------------------
;; Package Libraries and Directory Paths - Melpa, Marmalade, and El-Get 
;;--------------------------------------------------------------------------
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
(require 'cl) ;; common list 

;;--------------------------------------------------------------------------
;; Macros Keyboard Recipe  
;;--------------------------------------------------------------------------
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

;; occur key binding find all occurances of string
(global-set-key (kbd "C-c o") 'occur)

;;--------------------------------------------------------------------------
;; El-Get Package found under recipes  
;;--------------------------------------------------------------------------
(setq my:el-get-packages
      (append
       '(auto-complete auto-complete-c-headers buffer-move color-theme el-get
	 emacs-goodies-el escreen fill-column-indicator fuzzy flymake gnus
	 ggtags iedit minimap notify package popup smex smooth-scrolling
	 switch-window volatile-highlights yasnippet smartparens undo-tree 
         whitespace company-mode golden-ratio rainbow-mode helm sr-speedbar)))

(el-get 'sync my:el-get-packages)

;;--------------------------------------------------------------------------
;; Package Bookmark - sets bookmaks to files and location to open later 
;;--------------------------------------------------------------------------
(require 'bookmark) 
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*") ;; on startup Emacs displays Bookmarks

;;--------------------------------------------------------------------------
;; Package Interactively Do Things - minibuffer 
;;--------------------------------------------------------------------------
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

;;--------------------------------------------------------------------------
;; Screen and Window Management  
;;--------------------------------------------------------------------------
(require 'smooth-scrolling)
(require 'uniquify) ;; creates unique buffer names
(require 'ansi-color) 
(setq inhibit-startup-message t) ;; no startup message at startup
(setq inhibit-splash-screen t) ;; no splash screen at startup
(setq-default indent-tabs-mode nil) ;; no tabs
(menu-bar-mode -1) ;; no menu toolbar 
(display-time-mode t) ;; adds time to bottom window 
(column-number-mode t)
(mouse-avoidance-mode 'banish) ;; hides mouse cursor when not moved

;; insert line numbers
(global-linum-mode 1) ;; add line numbers to the left
(global-hl-line-mode t) ;; highlight current line

;; aligns the line numbers to the right
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)

;; set custom theme load path  
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/replace-colorthemes/"))

;; load theme
(load-theme 'tty-dark t t)
(enable-theme 'tty-dark)

;;(setq color-theme-is-global t) 
;;(color-theme-xemacs)

;; Buffer Navigation
(windmove-default-keybindings 'meta) ;; navigate buffers with M-<arros>
(setq windmove-wrap-around t)

;; Fullscreen
(defun fullscreen () 
  (interactive)
  (set-frame-parameter nil 'fullscreen 
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;;golden ratio package 
(require 'golden-ratio)
(golden-ratio-mode t) ;; allows for automatic buffer size based on current window 

;; wraps lines around in a nice way
(global-visual-line-mode 1)

(custom-set-faces ;;custom settings  
 '(flymake-errline ((((class color) (background light)) (:background "Red"))))
 '(highlight ((((class color) (min-colors 8)) 
	       (:background "white" :foreground "magenta"))))
 '(linum ((t (:foreground "black" :weight bold))))
 '(vertical-border ((t nil))))

;; rainbow-mode package sets background color to string matches
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; Package Fill Column Indicator  
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-color "darkblue")
(setq fci-rule-column 80)

;; replace list-buffers with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-use-other-window t) ;; display buffer in another window

;; Ctrl X-Ctrl S button dangerously close to Ctrl X-Ctrl C
(setq confirm-kill-emacs 'yes-or-no-p)

;;--------------------------------------------------------------------------
;; Buffer Edit Programming Tools  
;;--------------------------------------------------------------------------
(ac-config-default) ;; auto-complete global
(yas-global-mode 1) ;; yasnippet global
(require 'flymake)
;;(normal-erase-is-backspace-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

;; Company is a text completion framework, to use on all buffers 
(add-hook 'after-init-hook 'global-company-mode)

;; Package: smartparens
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

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
(setq compilation-ask-about-save nil ;; save before compiling
      compilation-always-kill t ;; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error) ;; Automatically scroll to first
(global-set-key (kbd "<f5>") 'compile)

;; Makefile
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))

;;--------------------------------------------------------------------------
;; File management settings  
;;--------------------------------------------------------------------------
(require 'ffap) ;; Find File at Point - used to open a directory path 
(require 'dired-x) ;; (Di)rectory (Ed)itor - for file management

(require 'recentf) ;; recent files
(setq
 recentf-max-menu-items 30
 recentf-max-saved-items 5000
)

;; save all backups in one folder
(setq backup-directory-alist '(("."."~/.emacs.d/saves")))
 
(setq large-file-warning-threshold 100000000) ;; size in bytes

(setq 
 make-backup-files t ;; backup a file the first time it is saved
 backup-by-copying t ;; copy the current file into backup directory
 version-control t ;; version numbers for backup files
 delete-old-versions t ;; delete unnecessary versions
 kept-old-versions 6 ;; oldest versions to keep (default: 2)
 kept-new-versions 9 ;; newest versions to keep (default: 2)
 auto-save-default t ;; auto-save every buffer that visits a file
 auto-save-timeout 20 ;; idle time before auto-save (default: 30)
 auto-save-interval 200 ;; number of keystrokes between auto-saves (default: 300)
)

;; Undo-tree - editing - view whole history of editing in a tree
(require 'undo-tree)
(global-undo-tree-mode t)

(require 'magit)

;;--------------------------------------------------------------------------
;; Helm Package Settings - still revising  
;;--------------------------------------------------------------------------
(require 'helm-config)
(require 'helm-grep)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq 
 helm-quick-update t ;; do not display invisible candidates
 helm-split-window-in-side-p t ;; open helm buffer inside current window
 helm-buffers-fuzzy-matching t ;; fuzzy matching buffer names when non--nil
 helm-move-to-line-cycle-in-source t ;; move to end or beginning of source when reaching top or bottom of source.
 helm-ff-search-library-in-sexp t ;; search for library in `require' and `declare-function' sexp.
 helm-scroll-amount 8 ;; scroll 8 lines other window using M-<next>/M-<prior>
 helm-ff-file-name-history-use-recentf t
)

(helm-mode 1)

;;--------------------------------------------------------------------------
;; c-cc- Programming Mode - still revising   
;;--------------------------------------------------------------------------
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(add-hook 'dired-mode-hook 'ggtags-mode)

;; add the bottom line to a set-up-programming.el to separate package  
;;(provide 'setup-programming)
;; GROUP: Programming -> Languages -> C
;; Available C styles: "gnu" "k&r" "bsd" "whitesmith" "stroustrup" "ellemetel"
;; "linux" "python" "java" "user"
(setq c-default-style "gnu" ; set to "gnu"
      c-basic-offset 4)

;; Programming -> Tools -> Gdb ;;
(setq gdb-many-windows t ; use gdb-many-windows by default
      gdb-show-main t) ; Non-nil means display source file containing the main routine at startup

;;--------------------------------------------------------------------------
;; Start Up Notiification - notifies once Emacs is up and load time   
;;--------------------------------------------------------------------------
(defun dim:notify-startup-done ()
  " notify user that Emacs is now ready"
  (el-get-notify
   "Emacs is ready."
   (format "The init sequence took %g seconds."
	      (float-time (time-subtract after-init-time before-init-time)))))

(add-hook 'after-init-hook 'dim:notify-startup-done)
