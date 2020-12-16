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

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

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

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(column-number-mode)

; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer my/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my/ctrl-c-keys
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

(global-set-key (kbd "C-M-u") 'universal-argument)

(defun my/evil-hook ()
  (dolist (mode '(custom-mode
                  git-rebase-mode
                  sauron-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (add-hook 'evil-mode-hook 'my/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package use-package-chords
  :disabled
  :config (key-chord-mode 1))

(use-package hydra
  :defer 1)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-prescient
  :init
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
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   ("C-M-l" . counsel-imenu)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
  :defer 1
  :after counsel)

(use-package wgrep)

(my/leader-key-def
 "r"   '(ivy-resume :which-key "ivy resume")
 "f"   '(:ignore t :which-key "files")
 "ff"  '(counsel-find-file :which-key "open file")
 "C-f" 'counsel-find-file
 "fr"  '(counsel-recentf :which-key "recent files")
 "fR"  '(revert-buffer :which-key "revert file")
 "fj"  '(counsel-file-jump :which-key "jump to file"))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))



(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "r" 'winner-redo)

  (use-package lsp-mode
    :defer t
    :commands lsp
    :bind (:map lsp-mode-map
		("C-c C-f" . lsp-format-buffer))
    :hook ((java-mode python-mode go-mode
	    js-mode js2-mode typescript-mode web-mode
	    c-mode c++-mode objc-mode) . lsp)
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
    :custom-face
    (lsp-ui-doc-background ((t (:background nil))))
    (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
    :custom
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

  (use-package company
    :diminish company-mode
    :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
    :bind
    (:map company-active-map
	  ([tab] . smarter-tab-to-complete)
	  ("TAB" . smarter-tab-to-complete))
    :custom
    (company-minimum-prefix-length 1)
    (company-tooltip-align-annotations t)
    (company-require-match 'never)
    ;; Don't use company in the following modes
    (company-global-modes '(not shell-mode eaf-mode))
    ;; Trigger completion immediately.
    (company-idle-delay 0.1)
    ;; Number the candidates (use M-1, M-2 etc to select completions).
    (company-show-numbers t)
    :config
    (global-company-mode 1)
    (defun smarter-tab-to-complete ()
      "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.

  If all failed, try to complete the common part with `company-complete-common'"
      (interactive)
      (if yas-minor-mode
	  (let ((old-point (point))
		(old-tick (buffer-chars-modified-tick))
		(func-list '(org-cycle yas-expand yas-next-field)))
	    (catch 'func-suceed
	      (dolist (func func-list)
		(ignore-errors (call-interactively func))
		(unless (and (eq old-point (point))
			     (eq old-tick (buffer-chars-modified-tick)))
		  (throw 'func-suceed t)))
	      (company-complete-common))))))

(use-package company-box
  :diminish
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

  (use-package smartparens
    :hook (prog-mode . smartparens-mode)
    :diminish smartparens-mode
    :config
    ;; Stop pairing single quotes in elisp
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'org-mode "[" nil :actions nil))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org
  :load-path ("~/vendor/org-mode/lisp" "~/vendor/org-mode/contrib/lisp"))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :diminish t
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-ellipsis "  "))

(use-package org-superstar
  :diminish t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-block-separator "")
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

  (use-package org
    :diminish t
    :config
    (setq org-src-window-setup 'current-window)
    (setq org-edit-src-persistent-message nil)
    (setq org-src-fontify-natively t)
    (setq org-src-preserve-indentation t)
    (setq org-src-tab-acts-natively t)
    (setq org-edit-src-content-indentation 0)
    (setq org-hide-block-startup t))

  (use-package org
    :diminish
    :config
    (setq org-structure-template-alist
	'(("e" . "src emacs-lisp"))))

(defun my/org-babel-tangle-save ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-save
                                         'run-at-end 'only-in-org-mode)))

(use-package olivetti
  :diminish t
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(use-package toc-org
  :hook (org-mode . toc-org-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet writeroom-mode which-key wgrep use-package unicode-fonts undo-tree toc-org smex smartparens rainbow-delimiters page-break-lines org-superstar org-bullets olivetti lsp-ui ivy-rich ivy-prescient ivy-hydra highlight-indent-guides general format-all flycheck-posframe flycheck-grammarly flx evil-collection doom-themes doom-modeline diminish default-text-scale counsel company-tabnine company-box bufler auto-package-update amx ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-posframe-border-face ((t (:inherit default)))))
