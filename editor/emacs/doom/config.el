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

(setq doom-font                (font-spec :family "FiraCode Nerd Font" :size 14))
(setq doom-big-font            (font-spec :family "FiraCode Nerd Font" :size 16))
(setq doom-variable-pitch-font (font-spec :family "Overpass Nerd Font" :size 14))
(setq doom-serif-font          (font-spec :family "BlexMono Nerd Font" :weight 'light))

(setq doom-theme 'doom-palenight)
(delq! t custom-theme-load-path)

(display-time-mode 1)
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)
