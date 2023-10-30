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

(add-to-list 'exec-path "/Applications/CMake.app/Contents/bin")
(add-to-list 'exec-path "/Users/thony/.nix-profile/bin")
(setq sql-postgres-program "/opt/homebrew/opt/libpq/bin/psql")

(setq sql-port 12432) ;; Default MySQL port - Postgres Local
(setq sql-port 12433) ;; Default MySQL port - Postgres Staging
(setq sql-port 12434) ;; Default MySQL port - Postgres Production

;; doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;; + `doom-font' + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrains Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 13)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24))
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
(setq org-directory "~/git/roam")
(after! org
  (setq org-agenda-files '("~/git/roam/*.org"
                           "~/git/roam/agenda.org"
                           "~/git/roam/inbox.org"
                           "~/git/roam/daily/*.org")))
;;
;; Set projectile discover directory
(setq projectile-project-search-path '(("~/git" . 1)))

;; Set up code review tooling
(setq code-review-fill-column 80)
(setq code-review-auth-login-marker 'forge)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Set frame, window, and buffer styles
(add-to-list 'default-frame-alist '(internal-border-width . 7))
(set-window-buffer nil (current-buffer))
(setq fringe-mode 'left-only)

;; Fix MacOS Swedish keyboard layout
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; Format TS buffers on save
(setenv "NODE_PATH" "/opt/homebrew/lib/node_modules")
(setq-hook! 'typescript-mode-hook +format-with 'prettier)

;; Projectile
(setq projectile-switch-project-action #'projectile-dired)

;; Dired
(use-package dired-single)

; (use-package dired
;   :ensure nil
;   :commands (dired dired-jump)
;   :bind (("C-x C-j" . dired-jump))
;   :custom ((dired-listing-switches "-agho --group-directories-first"))
;   :config
;   (evil-collection-define-key 'normal 'dired-mode-map
;     "h" 'dired-single-up-directory
;     "-" 'dired-single-up-directory
;     "l" 'dired-single-buffer
;     "o" 'dired-single-buffer))

;; Org
(map! :leader
      :desc "Execute code block"
      "c r" #'org-babel-execute-src-block)

;; Org Configuration
(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq org-image-actual-width 600))

(use-package! org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1))))

;; Make sure org-indent face is available
;; (require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(with-eval-after-load 'org-faces
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/git/roam"))
  (org-id-locations-file (file-truename "~/git/roam/.orgids"))
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
                     :templates '(("i" "Inbox" plain "* TODO %<%Y-%m-%d>: %?"
                                  :if-new (file+head "inbox.org" "#+title: Inbox\n")))))
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
