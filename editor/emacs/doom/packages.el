;; -*- no-byte-compile: t; -*-

(package! elcord)

(package! bufler)

(package! evil-better-visual-line)

(package! info-colors)

(package! general)

(package! magit-delta
  :recipe (:host github
           :repo "dandavison/magit-delta"))

(package! magithub)

(package! magit-org-todos)
(package! magit-todos)

(package! package-lint)
(package! flycheck-package)

(package! nov)

(package! calibredb)

(package! doct
  :recipe (:host github
           :repo "progfolio/doct"))

(package! org-super-agenda)

(package! org-pretty-table-mode
  :recipe (:host github
           :repo "Fuco1/org-pretty-table"))

(package! org-pretty-tags)

(package! org-download
  :recipe (:host github
           :repo "abo-abo/org-download"))

(package! org-graph-view
  :recipe (:host github
           :repo "alphapapa/org-graph-view"))

(package! helm-org-rifle)

(package! org-noter)

(package! org-protocol-capture-html
  :recipe (:host github
           :repo "alphapapa/org-protocol-capture-html"))

(package! org-ref)

(package! org-roam-bibtex)

(package! zotxt)

(package! vlf
  :recipe (:host github
           :repo "m00natic/vlfi"
           :files ("*.el"))
  :disable t)

(use-package! vlf-setup
  :defer-incrementally
  vlf-tune
  vlf-base
  vlf-write
  vlf-search
  vlf-occur
  vlf-follow
  vlf-ediff
  vlf)
