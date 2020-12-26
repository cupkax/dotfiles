;;; config.el -*- lexical-binding: t; -*-

(setq-default delete-by-moving-to-trash t)
(setq-default window-combination-resize t)
(setq-default x-stretch-cursor t)

(setq undo-limit 80000000)
(setq evil-want-fine-undo t)
(setq auto-save-default t)

(global-subword-mode 1)

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq user-full-name    "Vedant Sansare")
(setq user-mail-address "vedantsansare23@gmail.com")

(setq doom-font                (font-spec :family "FiraCode Nerd Font" :size 16))
(setq doom-big-font            (font-spec :family "FiraCode Nerd Font" :size 20))
(setq doom-variable-pitch-font (font-spec :family "Overpass Nerd Font" :size 16))
(setq doom-serif-font          (font-spec :family "BlexMono Nerd Font" :weight 'light))

(setq doom-theme 'doom-palenight)
(delq! t custom-theme-load-path)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "grey"))

(display-time-mode 1)

;(unless (equal "Battery status not available"
;               (battery))
;  (display-battery-mode 1))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(use-package! general)
;; Creating a constant for making future changes simpler
(defconst my-leader "SPC")
;; Tell general all about it
(general-create-definer my-leader-def
  :prefix my-leader)

;; I like short names
(general-evil-setup t)
;; Stop telling me things begin with non-prefix keys
(general-auto-unbind-keys)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name      "► Doom")

(use-package! bufler
  :general
  (:keymaps 'doom-leader-map
   "b b" 'bufler-workspace-switch-buffer
   "b B" 'bufler-switch-buffer)
  :config
  (setq bufler-workspace-switch-buffer-sets-workspace t))

(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-change-fonts "P22 Underground Book" 160))

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(setq elcord-use-major-mode-as-main-icon t)
