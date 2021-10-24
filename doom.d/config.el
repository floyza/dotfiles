;;; $DOOMDIR/config.el --- My private config -*- lexical-binding: t; -*-

;;; Identification
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gavin Downard"
      user-mail-address "gavin.downard@runbox.com")

;; (set-email-account! "runbox.com"
;;                     '((smtpmail-smtp-user . "gavin.downard@runbox.com"))
;;                     t)

;; nix-doom-emacs does not add this to the load-path
;; (add-to-list 'load-path "/etc/profiles/per-user/gavin/share/emacs/site-lisp/mu4e")

(setq send-mail-function    'smtpmail-send-it
      smtpmail-smtp-server  "mail.runbox.com"
      smtpmail-stream-type  'ssl
      smtpmail-smtp-service 465)

(after! circe
  (set-irc-server! "irc.libera.chat"
                   `(:tls t
                     :port 6697
                     :channels '("#emacs" "#haskell" "#osdev")
                     :nick "gdown"
                     :sasl-username "gdown"
                     :sasl-password (lambda (&rest _) (+pass-get-secret "irc/libera.chat")))))

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

(setq doom-font (font-spec :family "JetBrains Mono" :size 13 :weight 'semi-bold))
(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type t)

;;; evil config
(map! :i "C-w" evil-window-map) ; YES!!!!!
(after! vterm
  (map! :map vterm-mode-map :i "C-w" evil-window-map)) ; YASSS!
(map! :leader "C" #'calc-dispatch)

(map! :n "C-J" #'+workspace/switch-right)
(map! :n "C-K" #'+workspace/switch-left)

(map! :leader :n ":" #'pp-eval-expression)
(map! :leader :n ";" #'counsel-M-x)

(map! :map company-active-map "<return>" nil)
(map! :map company-active-map "RET" nil)
(map! :map company-active-map "C-<return>" #'company-complete-selection)
(map! :map company-active-map "C-RET" #'company-complete-selection)

;;; org-mode configuration

(setq org-directory "~/syncthing/org/")
(defun make-youtube-link (youtube_id)
  "Create a youtube link from an the id `YOUTUBE_ID'."
  (browse-url (concat "https://www.youtube.com/embed/" youtube_id)))
(defun evil-org-edit-src-exit ()
  "Call `org-edit-src-exit'."
  (interactive)
  (call-interactively #'org-edit-src-exit))
(after! org
  ;; (map! :map org-src-mode-map "Z Z" #'org-edit-src-exit)
  (org-add-link-type "yt" #'make-youtube-link)
   (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "inbox.org" "Inbox")
           "* TODO %?\n%a" :prepend t)
          ("T" "Tickler" entry
           (file "tickler.org")
           "* %^t %?\n%a" :prepend t)
;;; Notes are recorded in org-roam
          ;; ("n" "Personal notes" entry
          ;;  (file+headline +org-capture-notes-file "Inbox")
          ;;  "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)
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
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))
  ;; Org-roam
  ;; (setq org-roam-dailies-capture-templates
  ;;       `(("d" "default" entry (function org-roam-capture--get-point)
  ;;          "* %?"
  ;;          :file-name ,(concat org-roam-dailies-directory "%<%Y-%m-%d>")
  ;;          :head "#+title: %<%Y-%m-%d>\n")
  ;;         ("s" "school" entry (function org-roam-capture--get-point)
  ;;          "* %?"
  ;;          :file-name ,(concat org-roam-dailies-directory "school/%<%Y-%m-%d>")
  ;;          :head "#+title: Schoolwork %<%Y-%m-%d>\n")))
  )

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
                  (append lsp-file-watch-ignored-directories-additional (funcall orig))))

  ;; use `lsp-file-watch-ignored-directories' function instead of variable
  (defun lsp--get-ignored-regexes-for-workspace-root (workspace-root)
    "Return a list of the form (lsp-file-watch-ignored-files lsp-file-watch-ignored-directories) for the given WORKSPACE-ROOT."
    ;; The intent of this function is to provide per-root workspace-level customization of the
    ;; lsp-file-watch-ignored-directories and lsp-file-watch-ignored-files variables.
    (lsp--with-workspace-temp-buffer workspace-root
      (list lsp-file-watch-ignored-files (lsp-file-watch-ignored-directories)))))

;; (c-add-style "user"
;;              '("doom"
;;                (c-offsets-alist . ((innamespace . [0])))))

;; (after! scheme
;;   (setq geiser-repl-skip-version-check-p t
;;         geiser-active-implementations '(guile racket chicken chez mit chibi gambit)))

(after! scheme
  (setq geiser-repl-skip-version-check-p t))
(after! haskell
  (setq haskell-interactive-popup-errors nil))
(add-hook! lisp-mode
  (setq! inferior-lisp-program "common-lisp.sh"))

;; (after! python
;;   (set-repl-handler! 'python-mode #'+python/open-ipython-repl :persist t))

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
                              slurp/barf-lispy additional additional-insert)))

;; (setq-default indent-tabs-mode t)
(setq-default tab-width 2)

(after! evil
  (evil-escape-mode -1))
;; (setq! ivy-posframe-style 'frame-top-center)

;;; pdf-view mode
;; (add-hook! 'pdf-view-mode-hook :append #'pdf-view-midnight-minor-mode)

(after! flycheck
  (setq flycheck-global-modes '(not nix-mode)))

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Fix emacs exec-path
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defun minibuffer-set-register (register)
  (interactive (list (register-read-with-preview "Set register: ")))
  (set-register register (read-string "Set: ")))

(map! :leader :n "r s" #'minibuffer-set-register)
(map! :leader :n "r i" #'insert-register)

;; `use-package!' declarations
;; see `packages.el' for info on packages

(use-package! hackernews)
(use-package! egg-timer)
(use-package! saveplace-pdf-view)
(use-package! disk-usage)
(use-package! odin-mode)

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
