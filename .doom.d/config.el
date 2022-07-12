;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "cupkax"
      user-mail-address "vedant.sansare@protonmail.com")

(setq inhibit-compacting-font-caches t
      undo-limit 80000000
      load-prefer-newer t
      evil-want-fine-undo t
      global-subword-mode 1
      delete-by-moving-to-trash t
      tab-width 4
      indent-tabs-mode nil
      uniquify-buffer-name-style 'forward
      window-combination-resize t
      x-stretch-cursor nil
      scroll-preserve-screen-position 'always)
(pixel-scroll-precision-mode 1)
(general-auto-unbind-keys :off)
(remove-hook 'doom-after-init-modules-hook #'general-auto-unbind-keys)
(whitespace-mode -1)
(global-auto-revert-mode 1)

(setq-default custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(defun cpkx/macwin-to-unix ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(add-hook 'find-file-hook 'cpkx/macwin-to-unix)

(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program cmd-exe
            browse-url-generic-args cmd-exe
            browse-url-browser-function 'browse-url-generic))))

(setq doom-font (font-spec :family "DankMono NF" :size 16)
      doom-big-font (font-spec :family "DankMono NF" :size 24)
      doom-unicode-font (font-spec :family "JuliaMono"))
(setq doom-font-increment 1)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(setq display-time-day-and-date t)
(display-time-mode 1)

(setq display-line-numbers-type nil)
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(after! smartparens
  (show-smartparens-global-mode 1))

(setq deft-directory "~/org/roam/")

(setq ispell-dictionary "en-custom"
      company-ispell-dictionary "en-custom"
      ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2)
  (setq-default history-length 1000))
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(setq org-re-reveal-root "/home/vedamt/reveal.js"
      org-re-reveal-theme "white"
      org-re-reveal-transition "slide"
      org-re-reveal-plugins '(markdown notes math search zoom))
