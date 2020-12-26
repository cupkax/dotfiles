;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company
        +childframe)
       ;;helm
       ;;ido
       (ivy
        +childframe
        +prescient
        +icons
        +fuzzy)

       :ui
       deft
       doom
       doom-dashboard
       doom-quit
       fill-column
       hl-todo
       hydra
       indent-guides
       (ligatures
        +extra)
       modeline
       nav-flash
       ophints
       (popup
        +all
        +defaults)
       (tabs                      ; an tab bar for Emacs
         +centaur-tabs)           ; ... with prettier tabs
       treemacs
       ;;unicode                    ; extended unicode support for various languages
       vc-gutter
       vi-tilde-fringe
       (window-select
        +numbers)
       workspaces
       ;;zen                          ; distraction-free coding or writing

       :editor
       (evil
        +everywhere)
       file-templates
       fold
       (format
        +onsave)
       multiple-cursors
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired
        +ranger
        +icons)
       electric
       (ibuffer +icons)
       (undo +tree)
       vc

       :term
       vterm

       :checkers
       (syntax
        +childframe)
       spell
       grammar

       :tools
       direnv
       editorconfig
       ;;ein                        ; tame Jupyter notebooks with emacs
       (eval +overlay)
       gist
       (lookup
        +offline
        +dictionary
        +docsets)
       lsp
       (magit
        +forge)
       make
       ;;pass                       ; password manager for nerds
       pdf
       rgb

       :os
       tty

       :lang
       (csharp
        +lsp)
       data
       emacs-lisp
       json
       (javascript +lsp)
       ;;ledger                      ; an accounting system in Emacs
       lua
       markdown
       ;;nix
       (org
        ;;+jupyter                   ; ipython/jupyter support for babel
        ;;+pomodoro                  ; be fruitful with the tomato technique
        ;;+present                   ; using org-mode for presentations
        +dragndrop                   ; drag & drop files/images into org buffers
        +gnuplot                     ; who doesn't like pretty pictures
        +hugo                        ; use Emacs for hugo blogging
        +pandoc                      ; export-with-pandoc support
        +pretty                      ; yessss my pretties! (nice unicode symbols)
        +roam)                       ; wander around notes
       (python
        +poetry
        +lsp)
       ;;scheme                      ; a fully conniving family of lisps
       sh
       web
       yaml

       :email
       (mu4e +org +gmail)

       :app
       ;;calendar
       ;;irc                          ; how neckbeards socialize
       ;;(rss +org)                   ; emacs as an RSS reader
       ;;twitter                    ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens)
       )
