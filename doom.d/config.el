;;; $DOOMDIR/config.el --- My private config -*- lexical-binding: t; -*-

;;; Identification
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gavin Downard"
      user-mail-address "gavin.downard@runbox.com")

(setq auth-sources '())
(auth-source-pass-enable)
(setq auth-source-pass-filename "~/.local/share/password-store")

(after! mu4e
  (setq send-mail-function    'smtpmail-send-it
        smtpmail-smtp-user user-mail-address
        smtpmail-smtp-server  "mail.runbox.com"
        smtpmail-stream-type  'starttls
        smtpmail-servers-requiring-authorization ".*" ; Ordinarily it attempts to connect without credentials before using credentials, but that breaks the whole thing, at least with runbox.
        smtpmail-smtp-service 587
        +mu4e-alert-bell-cmd nil))

(setq auth-source-pass-filename "~/.local/share/password-store")

(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :channels ("#emacs" "#haskell" "#nixos") ; #osdev
      :nick "gdown"
      :sasl-username "gdown"
      :sasl-password (lambda (&rest _) (+pass-get-secret "irc/libera.chat"))))
  (set-irc-server! "irc.pine64.org"
    `(:tls t
      :port 6697
      :channels ("#pine64" "#pinetime" "#pinetime-dev")
      :nick "gdown")))

;;; Visible changes

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "JetBrains Mono" :size 11.0 :weight 'normal))
(setq doom-theme 'leuven)
(setq display-line-numbers-type t)

(setq next-error-message-highlight t
      read-minibuffer-restore-windows nil
      mouse-wheel-progressive-speed nil)

(after! projectile
  (setq projectile-track-known-projects-automatically nil)
  (add-to-list 'projectile-globally-ignored-directories "zig-cache")
  (add-to-list 'projectile-globally-ignored-directories "zig-out")
  (add-to-list 'projectile-globally-ignored-directories "dist-newstyle")
  (add-to-list 'projectile-globally-ignored-directories ".stack-work"))

;;; evil config
(setq evil-move-cursor-back nil         ; typing in insert mode -> ESC -> org-insert-link
      evil-move-beyond-eol t)
(map! :i "C-w" evil-window-map) ; YES!!!!!
(after! vterm
  (map! :map vterm-mode-map :i "C-w" evil-window-map)) ; YASSS!
(map! :leader "C" #'calc-dispatch)

(map! :leader :n ":" #'pp-eval-expression)
(map! :leader :n ";" #'counsel-M-x)

(map! :map company-active-map "<return>" nil)
(map! :map company-active-map "RET" nil)
(map! :map company-active-map "C-<return>" #'company-complete-selection)
(map! :map company-active-map "C-RET" #'company-complete-selection)

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;; org-mode configuration

(setq org-directory "~/my/org/")
(defun evil-org-edit-src-exit ()
  "Call `org-edit-src-exit'."
  (interactive)
  (call-interactively #'org-edit-src-exit))
(after! org
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "inbox.org" "Inbox")
           "* TODO %?\n%a" :prepend t)
          ("T" "Tickler" entry
           (file "tickler.org")
           "* %^t %?\n%a" :prepend t)
          ("j" "School Log" entry
           (file+olp+datetree "school-log.org")
           "* %?" :unnarrowed t)
          ("J" "School Log" entry
           (file+olp+datetree "school-log.org")
           "* %?" :unnarrowed t :time-prompt t)
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))))

(assq-delete-all ?* +ligatures-composition-alist)
(setq +ligatures-extra-symbols (doom-plist-delete +ligatures-extra-symbols
                                                  :true
                                                  :false
                                                  :bool
                                                  :list
                                                  :pipe))

(set-ligatures! 'haskell-mode
  :lambda "\\")

(map! :map doom-leader-notes-map
      :desc "Capture arbitrary date" "r d d" #'org-roam-dailies-capture-date
      :desc "Capture tomorrow"       "r d m" #'org-roam-dailies-capture-tomorrow
      :desc "Capture today"          "r d t" #'org-roam-dailies-capture-today
      :desc "Capture yesterday"      "r d y" #'org-roam-dailies-capture-yesterday
      :desc "Find arbitrary date"    "r d D" #'org-roam-dailies-find-date
      :desc "Find tomorrow"          "r d M" #'org-roam-dailies-find-tomorrow
      :desc "Find today"             "r d T" #'org-roam-dailies-find-today
      :desc "Find yesterday"         "r d Y" #'org-roam-dailies-find-yesterday)


(defvar lsp-file-watch-ignored-directories-additional nil
  "Additional ignored directories added to lsp-file-watch-ignored-directories.")
(put 'lsp-file-watch-ignored-directories-additional 'safe-local-variable #'listp)
(after! lsp-mode
  (add-function :around (symbol-function 'lsp-file-watch-ignored-directories)
                (lambda (orig)
                  (append lsp-file-watch-ignored-directories-additional (funcall orig)))))

(after! lsp-ui
  (setq! lsp-ui-sideline-show-code-actions t
         lsp-ui-doc-show-with-cursor t
         lsp-ui-doc-delay 0.2))

(after! scheme
  (setq geiser-repl-skip-version-check-p t))

(setq debug-on-error t)                 ; workaround bug in haskell-mode inferior lol
(after! haskell
  (setq lsp-haskell-formatting-provider "ormolu")
  (setq haskell-interactive-popup-errors nil)
  (defun haskell-hoogle-lookup ()
    (interactive)
    (haskell-hoogle-start-server)
    (haskell-hoogle-lookup-from-local))
  (map! :localleader
        :map haskell-mode-map
        "l" #'haskell-hoogle-lookup))

(add-hook! lisp-mode
  (setq! inferior-lisp-program "common-lisp.sh"))

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(after! counsel
  (setq counsel-compile-local-builds
        '(counsel-compile-get-filtered-history counsel-compile-get-build-directories) ;; counsel-compile-get-make-invocation counsel-compile-get-make-help-invocations
        compile-command "nix-build "))

(after! company
  (setq company-idle-delay 0.2
        company-tooltip-idle-delay 0.2))

(after! lispy
  ;; lispy-mode-map-base uses copy-keymap instead of inheritance,
  ;; so we need to copy to each keymap individually
  (map! :map (lispy-mode-map-paredit lispy-mode-map-parinfer lispy-mode-map-evilcp lispy-mode-map-lispy)
        "<C-backspace>" 'lispy-backward-kill-word)
  (lispyville-set-key-theme '((operators normal)
                              c-w
                              c-u
                              commentary
                              (additional-wrap normal insert)
                              additional-movement ; could put under visual only
                              (prettify insert)
                              (atom-movement t)
                              slurp/barf-lispy additional additional-insert))
  (add-to-list 'lispy-elisp-modes 'minibuffer-mode)
  (add-to-list 'lispy-no-indent-modes 'minibuffer-mode))

;; (setq-default indent-tabs-mode t)
(setq-default tab-width 2)

(after! evil
  (evil-escape-mode -1))
;; (setq! ivy-posframe-style 'frame-top-center)

(after! flycheck
  (setq flycheck-global-modes '(not nix-mode)))

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(defun minibuffer-set-register (register)
  (interactive (list (register-read-with-preview "Set register: ")))
  (set-register register (read-string "Set: ")))

(map! :leader :n "r s" #'minibuffer-set-register)
(map! :leader :n "r i" #'insert-register)

;; `use-package!' declarations
;; see `packages.el' for info on packages

(setq elfeed-feeds
      '("https://hnrss.org/frontpage?comments=25"
        "https://hnrss.org/bestcomments"
        "https://www.youtube.com/feeds/videos.xml?channel_id=UC3ts8coMP645hZw9JSD3pqQ"
        "https://factorio.com/blog/rss"
        "https://xkcd.com/rss.xml"))
(after! elfeed
  (add-hook! 'elfeed-search-mode-hook #'elfeed-update))

(use-package! hackernews)
(use-package! egg-timer)
(use-package! saveplace-pdf-view)
(use-package! disk-usage)
(use-package! sdcv-mode)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (pushnew! tree-sitter-major-mode-language-alist
            '(haskell-mode . haskell)))

;;; Defuns

(defun nix-generate-project ()
  (interactive)
  (let ((buffer (find-file "./.envrc")))
    (insert "use nix")
    (save-buffer)
    (kill-buffer buffer)))

(defun my-buffer-local-set-key (key command)
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))

(defun update-turns--modify (name file location)
    (save-window-excursion
      (find-file file)
      (goto-char location)
      (org-end-of-subtree)
      (org-insert-item)
      (condition-case err
          (org-time-stamp-inactive)
        (quit
         (delete-region (line-beginning-position) (1+ (line-end-position))))
        (:success
         (end-of-line)
         (insert name)))
      (save-buffer)))

(defun update-turns (name)
  "Update the turns org file."
  (interactive "MName of person: ")
  (doom-completing-read-org-headings "Task: " "~/my/turns.org" :depth 1 :action (-cut update-turns--modify name <> <>)))

