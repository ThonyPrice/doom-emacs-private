;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thony Price"
      user-mail-address "thonyprice@gmail.com")

;; Set package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(add-to-list 'exec-path "/Users/thony/.nix-profile/bin")

;; Aid Magit knowing what terminal to run git in,
;; See: https://github.com/magit/magit/issues/4209#issuecomment-1698136735
(setenv "TERM" "xterm-kitty")

;; Copilot setup - Accept completion from copilot, to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;; + `doom-font' + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono" :size 14)
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'macchiato) ;; or 'frappe, 'latte, 'macchiato, or 'mocha

;; If you use `org' and don'trwant your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/git/org")
(after! org
  (setq org-agenda-files (directory-files-recursively "~/git/org/" "\\.org$"))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(n)"  ; A task that should be done next
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("KILL" . +org-todo-cancel))))


;; Projectile
(setq projectile-switch-project-action #'projectile-dired)
(setq projectile-project-search-path '(("~/git" . 1)))

;; Set up code review tooling
(setq code-review-fill-column 80)
(setq code-review-auth-login-marker 'forge)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(modify-all-frames-parameters
 '((right-divider-width . 8)
   (internal-border-width . 8)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(setq fringe-mode 'left-only)

;; Fix MacOS Swedish keyboard layout
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; Org
(map! :leader
      :desc "Execute code block"
      "c r" #'org-babel-execute-src-block)

;; Org Configuration
(defun org-mode-setup ()
  (auto-fill-mode 0)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (global-org-modern-mode)
  (setq org-hide-emphasis-markers t
        org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-auto-align-tags nil
        org-tags-column 0
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        ;; Org styling, hide markup etc.
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-agenda-tags-column 0

        evil-auto-indent nil
        org-image-actual-width 600

        org-modern-fold-stars
        '(("◉" . "◉")
          ("○" . "○")
          ("●" . "●")
          ("○" . "○")
          ("●" . "●"))))

(use-package! org
  :hook (org-mode . org-mode-setup))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/git/org"))
  (org-id-locations-file (file-truename "~/git/org/.orgids"))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (setq org-roam-db-autosync-mode t))

;; Org Roam Graph
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Org Capture

;; Capture to Inbox
(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "Inbox" plain "* TODO %?\n:PROPERTIES:\n:DATE_ADDED: %t\n:END:"
                                   :if-new (file "00_GTD/inbox.org")
                                   :empty-lines-before 1)
                                  ("w" "Work" plain "* TODO %? :@work:\n:PROPERTIES:\n:DATE_ADDED: %t\n:END:"
                                   :if-new (file "00_GTD/inbox.org")
                                   :empty-lines-before 1)
                                  ("p" "Project" plain "* PROJ %? [%]\n:PROPERTIES:\n:ORDERED: t\n:DATE_ADDED: %t\n:END:\n** TODO ..."
                                   :if-new (file "00_GTD/projects.org")
                                   :empty-lines-before 1))))

(map! :leader
      :desc "org-roam-gtd-inbox"
      "n r c" #'my/org-roam-capture-inbox)

;; Capture Helpers
(defun my/delete-capture-frame (&rest _)
  "Delete frame with its name frame-parameter set to \"capture\"."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))
(advice-add 'org-capture-finalize :after #'my/delete-capture-frame)

(defun my/org-capture-frame ()
  "Run org-capture in its own frame."
  (interactive)
  (require 'cl-lib)
  (select-frame-by-name "capture")
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (condition-case err
        (my/org-roam-capture-inbox)
      ;; "q" signals (error "Abort") in `org-capture'
      ;; delete the newly created frame in this scenario.
      (user-error (when (string= (cadr err) "Abort")
                    (delete-frame)))))
  (delete-other-windows)
  )

;; Org refile
(setq org-refile-targets '((("~/git/org/00_GTD/archive.org"
                             "~/git/org/00_GTD/main.org"
                             "~/git/org/00_GTD/projects.org") :maxlevel . 2)))

;; Org refile to top of headings
(setq org-reverse-note-order t)

;; Open GTD files
(map! :leader
      :prefix ("o g" . "GTD files")
      :desc "Inbox"
      "i" (lambda () (interactive) (find-file "~/git/org/00_GTD/inbox.org"))
      :desc "Mobile inbox"
      "I" (lambda () (interactive) (find-file "~/git/org/00_GTD/inbox-mobile.org"))
      :desc "Main"
      "m" (lambda () (interactive) (find-file "~/git/org/00_GTD/main.org"))
      :desc "Projects"
      "p" (lambda () (interactive) (find-file "~/git/org/00_GTD/projects.org"))
      :desc "Logs"
      ;; INFO: Soft link the current file to the latest log file:
      ;; $ ln -s 2025MM-logs.org current
      "l" (lambda () (interactive) (find-file "~/git/org/01_Logs/current")))

(setq org-duration-format 'h:mm)

;; GTD Agenda View
(setq org-agenda-custom-commands
      '(("g" "GTD Tasks"
         ((tags-todo
           "next"
           ((org-agenda-files '("~/git/org/00_GTD/next.org"))
            (org-agenda-overriding-header "Next")))
          (tags-todo
           "inbox"
           ((org-agenda-files '("~/git/org/00_GTD/inbox.org"))
            (org-agenda-overriding-header "Inbox")))
          (org-todo-list)))))


(setq org-tag-alist
      '(;; Places
        ("@home" . ?H)
        ("@work" . ?W)

        ;; Devices
        ("@computer" . ?C)
        ("@phone" . ?P)

        ;; Activities
        ("@planning" . ?n)
        ("@programming" . ?p)
        ("@writing" . ?w)
        ("@creative" . ?c)
        ("@email" . ?e)
        ("@calls" . ?a)
        ("@errands" . ?r)))

;; Org Presentations
(defun presentation-setup ()
  ;; Hide the mode line
  ;; (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Turn off spell check
  (flyspell-mode)

  ;; Scale the text.  The next line is for basic scaling:
                                        ; (setq text-scale-mode-amount 3)
                                        ; (text-scale-mode 1)


  ;; This option is more advanced, allows you to scale other faces too
  (setq-local face-remapping-alist '((default (:height 2.5) variable-pitch)
                                     (org-code (:height 2.25) org-code)
                                     (org-verbatim (:height 2.25) org-verbatim)
                                     (org-meta-line (:height 2.0) org-meta-line)
                                     (org-block (:height 2.0) org-block))))

(defun presentation-end ()
  ;; Show the mode line again
  ;; (hide-mode-line-mode 0)

  ;; Turn Spelling back on
  (flyspell-mode)

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
                                        ; (text-scale-mode 0)

  ;; If you use face-remapping-alist, this clears the scaling:
  (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package! org-tree-slide
  :hook ((org-tree-slide-play . presentation-setup)
         (org-tree-slide-stop . presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > "))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Debugger
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(with-eval-after-load 'ox
  (require 'ox-hugo))

(defun my/org-roam-export-all ()
  (interactive)
  (dolist (org-file (directory-files-recursively org-roam-directory "\.org$"))
    (with-current-buffer (find-file org-file)
      (message (format "[build] Exporting %s" org-file))
      (org-hugo-export-wim-to-md :all-subtrees nil nil nil))))

(map! :leader
      :desc "org-roam-export-all"
      "n h e" #'my/org-roam-export-all)
