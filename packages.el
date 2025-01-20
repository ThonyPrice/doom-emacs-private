;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; Doom package management documentation:
;; https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#package-management

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! org-bullets)
(package! org-modern)
(package! org-roam)
(package! org-roam-ui)
(package! org-super-agenda)
(package! org-tree-slide)
(package! ox-hugo)
(package! prettier)
(package! websocket)
(package! vue-mode)

