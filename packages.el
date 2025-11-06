;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; Pull ws-butler from NonGNU mirror branch to avoid cloning savannah
(package! ws-butler
  :recipe (:host github
           :repo "emacsmirror/nongnu_elpa"
           :branch "elpa/ws-butler"))

(unpin! ghub forge)

;; Doom package management documentation:
;; https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#package-management

(package! alert)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! org-bullets)
(package! org-clock-convenience)
(package! org-modern)
(package! org-roam)
(package! org-roam-ui)
(package! org-super-agenda)
(package! org-tree-slide)
(package! ox-hugo)
(package! claude-code
  :recipe (:host github
           :repo "stevemolitor/claude-code.el"
           :branch "main"
           :files ("*.el" (:exclude "images/*"))))
(package! monet
  :recipe (:host github
           :repo "stevemolitor/monet"
           :branch "main"
           :files ("*.el")))
(package! prettier)
(package! websocket)
(package! vue-mode)
(package! gptel)

