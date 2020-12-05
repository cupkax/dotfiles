;;; init.el --- -*- lexical-binding: t -*-

(defvar better-gc-cons-threshold 67108864)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold better-gc-cons-threshold)
	    (setq file-name-handler-alist file-name-handler-alist-original)
	    (makunbound 'file-name-handler-alist-original)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder)) 
   (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp" user-emacs-directory))

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst python-p
  (or (executable-find "python3")
      (and (executable-find "python")
           (> (length (shell-command-to-string "python --version | grep 'Python 3'")) 0)))
  "Do we have python3?")

(defconst pip-p
  (or (executable-find "pip3")
      (and (executable-find "pip")
           (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
  "Do we have pip3?")

(defconst clangd-p
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Do we have clangd?")

(defconst eaf-env-p
  (and *sys/linux* (display-graphic-p) python-p pip-p
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Do we have EAF environment setup?")

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package diminish)

(setq user-full-name "Vedant Sansare")
(setq user-mail-address "vedantsansare23@gmail.com")

(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t)               ; only for mouse events
  (setq inhibit-splash-screen t)
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-h h" . nil)))

;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar font-list '(("FiraCode Nerd Font" . 12) ("JetBrainsMono Nerd Font" . 12)))

(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
	(push font available-fonts)))
    (if (not available-fonts)
	(message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
	  (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
	    (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
	(setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(when (display-graphic-p)
  (change-font))

(use-package all-the-icons)

(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  (setq prettify-symbols-alist
	'(
	  ("lambda" . 955)
	  ("delta" . 120517)
	  ("epsilon" . 120518)
	  ("->" . 8594)
	  ("<=" . 8804)
	  (">=" . 8805)
	  )))
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)

(use-package doom-themes
  :config

(doom-themes-visual-bell-config)

(doom-themes-org-config)

(load-theme 'doom-dracula t)

(defun switch-theme ()
  "An interactive funtion to switch themes."
  (interactive)
  (disable-theme (intern (car (mapcar #'symbol-name custom-enabled-themes))))
  (call-interactively #'load-theme))

)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-height 15)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t))

(display-time-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (setq which-key-idle-delay 0)
  (which-key-mode))

(use-package org
  :load-path ("~/vendor/org-mode/lisp" "~/vendor/org-mode/contrib/lisp"))

(use-package org
  :config
  (setq org-structure-template-alist
        '(("e" . "src emacs-lisp"))))
