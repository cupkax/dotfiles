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

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

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

(defconst eaf-env-p
  (and *sys/linux* (display-graphic-p) python-p pip-p
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Do we have EAF environment setup?")

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(prefer-coding-system 'utf-8-unix)

(setenv "LANG" "en_GB")

(global-set-key (kbd "C-z") 'undo) ;Emacs default is bound to hide Emacs.
(global-set-key (kbd "C-SPC") nil)

(setq default-directory "~/" )

(setq package-archives
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

(use-package dash :ensure t)
(use-package diminish :ensure t)

(setq user-full-name "Vedant Sansare")
(setq user-mail-address "vedantsansare23@gmail.com")

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)
(tool-bar-mode 0) 
;(tooltip-mode  0)
(scroll-bar-mode 0)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; Set the font face based on platform
 (set-face-attribute 'default nil :font "FiraCode Nerd Font"  :height 110)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 110)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 120)

(defun my/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :ensure t
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
    (lambda (block-name)
      (my/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
    '("Dingbats"
      "Emoticons"
      "Miscellaneous Symbols and Pictographs"
      "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package all-the-icons)
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

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

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome Vedant")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil))

(setq custom-safe-themes t)
(use-package doom-themes
  :config
  ;Flash mode-line on error
  (doom-themes-visual-bell-config)

  ;Corrects org-mode’s native fontification
  (doom-themes-org-config)

  ;An interactive funtion to switch themes.
  (defun cpkx/switch-theme ()
  (interactive)
  (disable-theme (intern (car (mapcar #'symbol-name custom-enabled-themes))))
  (call-interactively #'load-theme))

  ;Set Theme
  (load-theme 'doom-dracula t))

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

(eval-after-load "face-remap"
  '(diminish 'buffer-face-mode))

(use-package general
  :config
  (general-create-definer cpkx/leader-key-def
    :prefix "C-SPC")

  (general-create-definer cpkx/ctrl-c-keys
    :prefix "C-c"))

(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (setq which-key-idle-delay 0)
  (which-key-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(winner-mode)

(use-package ivy
  :diminish
  :init
  (use-package counsel :defer t)
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("TAB" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'swiper ivy-height-alist) 15))


(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line))

(use-package counsel
  :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-M-j"   . 'counsel-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-M-l"   . counsel-imenu)
   :map minibuffer-local-map
   ("C-r"     . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)
  (counsel-mode 1))

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(cpkx/leader-key-def
  "f"   '(:ignore t :which-key "files")
  "fr"  '(counsel-recentf :which-key "recent files")
  "fR"  '(revert-buffer :which-key "revert file"))

(use-package company
  :diminish t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :diminish t
  :hook (company-mode . company-box-mode))

(defun cpkx/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . cpkx/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package flycheck
  :defer t
  :diminish
  :hook ((prog-mode markdown-mode) . flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not text-mode outline-mode fundamental-mode org-mode
         diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  :init
  (use-package flycheck-grammarly :defer t)
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :custom
        (flycheck-posframe-border-width 1)
        (flycheck-posframe-inhibit-functions
         '((lambda (&rest _) (bound-and-true-p company-backend)))))
    (use-package flycheck-pos-tip
      :defines flycheck-pos-tip-timeout
      :hook (flycheck-mode . flycheck-pos-tip-mode)
      :custom (flycheck-pos-tip-timeout 30)))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))

(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))

(use-package highlight-indent-guides
  :diminish
  :hook ((prog-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :diminish smartparens-mode
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil))

(show-paren-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org
  :load-path ("~/.emacs.d/site-packages/org-mode/lisp" "~/.emacs.d/site-packages/org-mode/contrib/lisp"))

(defun cpkx/org-mode-setup ()
  (org-indent-mode)
  (diminish 'org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0))

(use-package org
  :diminish t
  :hook (org-mode . cpkx/org-mode-setup)
  :config
  (setq org-directory "~/Dropbox/org"))

(use-package org
  :diminish
  :config
  (setq org-structure-template-alist
      '(("e" . "src emacs-lisp"))))

(defun cpkx/org-babel-tangle-save ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cpkx/org-babel-tangle-save
                                         'run-at-end 'only-in-org-mode)))

(use-package ivy-bibtex
  :defer t
  :config
  (setq bibtex-completion-bibliography    '("~/Dropbox/org/Research/PhD.bib"))
  (setq bibtex-completion-notes-path        "~/Dropbox/org/Research/Notes")
  (setq bibtex-completion-library-path      "~/Dropbox/org/Research/zotero-library/")
  (setq bibtex-completion-pdf-field "file"))

(defun cpkx/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(use-package org-ref
  :config
  (setq org-ref-default-bibliography      '("~/Dropbox/org/Research/PhD.bib"))
  (setq org-ref-bibliography-notes          "~/Dropbox/org/Research/notes.org")
  (setq org-ref-pdf-directory               "~/Dropbox/org/Research/zotero-library/")
  (setq org-ref-completion-library        'org-ref-ivy-cite)
  (setq org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-ivy-bibtex)
  (setq org-ref-open-pdf-function 'cpkx/org-ref-open-pdf-at-point))

(use-package org-noter
  :config
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf")))

(use-package olivetti
  :hook ((text-mode) . olivetti-mode)
  :diminish
  (olivetti-mode)
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))
(diminish 'visual-line-mode)

;;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 2)

(use-package pandoc-mode
  :hook ((text-mode) . pandoc-mode)
  :diminish pandoc-mode)

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(pdf-loader-install)

(server-start)
