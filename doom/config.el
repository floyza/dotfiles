;;; $DOOMDIR/config.el --- My private config -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.config/doom/libraries")

(require 's)
(require 'f)
(require 'google-c-style)

;;; Library of useful functions
(defun g/pp (&rest stuff)
  (pp stuff))

;;; Identification
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gavin Downard"
      user-mail-address "me@floyza.com")

(setq auth-sources '())
(auth-source-pass-enable)
(setq auth-source-pass-filename "~/.local/share/password-store")

(after! mu4e
  (setq send-mail-function    'smtpmail-send-it
        smtpmail-smtp-user "gavin.downard@runbox.com"
        smtpmail-smtp-server  "mail.runbox.com"
        smtpmail-stream-type  'starttls
        smtpmail-servers-requiring-authorization ".*" ; Ordinarily it attempts to connect without credentials before using credentials, but that breaks the whole thing, at least with runbox.
        smtpmail-smtp-service 587
        +mu4e-alert-bell-cmd nil))

(setq auth-source-pass-filename "~/.local/share/password-store")

(after! format
  ;; ech, better than nothing
  ;; (defun g/format-buffer-then-save ()
  ;;   (when
  ;;       (or (run-hook-with-args-until-success '+format-functions (point-min) (point-max) 'buffer)
  ;;           (apheleia-format-after-save))
  ;;     (sleep-for 0.1)
  ;;     (save-buffer)))
  ;; prioritize `+format-functions' (e.g. lsp) over using aphelieia proper
  ;; (add-hook 'apheleia-mode-hook (lambda ()
  ;;                                 (if apheleia-mode
  ;;                                     (progn
  ;;                                       (remove-hook 'after-save-hook #'apheleia-format-after-save 'local) ; take it away again!
  ;;                                       (add-hook 'after-save-hook #'g/format-buffer-then-save nil 'local))
  ;;                                   (remove-hook 'after-save-hook #'g/format-buffer-then-save 'local))))
  )

(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :channels ("#emacs" "#haskell" "#nixos" "#crawl" "#magicjudges-rules" "#raku") ; #osdev
      :nick "gdown"
      :sasl-username "gdown"
      :sasl-password (lambda (&rest _) (+pass-get-secret "irc/libera.chat"))))
  (set-irc-server! "irc.rizon.net"
    `(:tls t
      :port 6697
      :nick "gdown"
      :sasl-username "gdown"
      :sasl-password (lambda (&rest _) (+pass-get-secret "irc/rizon.net"))))
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
(setq doom-gruvbox-dark-variant "hard")
(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type t)

(setq next-error-message-highlight t
      read-minibuffer-restore-windows nil
      mouse-wheel-progressive-speed nil)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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

(map! :leader :desc "irc" "o i" #'=irc)

(map! :leader :n ":" #'pp-eval-expression)
(map! :leader :n ";" #'counsel-M-x)

(after! company
  (map! :map company-active-map "<return>" nil)
  (map! :map company-active-map "RET" nil)
  (map! :map company-active-map "C-<return>" #'company-complete-selection)
  (map! :map company-active-map "C-RET" #'company-complete-selection)
  ;; prevent interference with yasnippet: we use C-j + C-k instead anyways
  (add-hook 'company-mode-hook (lambda () (remove-hook 'yas-keymap-disable-hook 'company--active-p t))))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;; org-mode configuration

(setq org-directory "/home/gavin/docs/my/org/")
(defun evil-org-edit-src-exit ()
  "Call `org-edit-src-exit'."
  (interactive)
  (call-interactively #'org-edit-src-exit))
(after! org
  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil)))
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "inbox.org" "Inbox")
           "* TODO %?\n%a" :prepend t)
          ("T" "Tickler" entry
           (file "tickler.org")
           "* %^t %?\n%a" :prepend t)
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

;; remove missing symbols
(mapc (lambda (p) (cl-remf +ligatures-extra-symbols p)) '(:pipe))

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

(after! lsp-ui
  (setq! lsp-ui-sideline-show-code-actions t
         lsp-ui-doc-show-with-cursor t
         lsp-ui-doc-delay 0.2))

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

(after! scheme
  (setq geiser-repl-skip-version-check-p t))

;; (setq debug-on-error t)                 ; workaround bug in haskell-mode inferior lol
(after! haskell
  (setq lsp-haskell-formatting-provider "ormolu")
  ;; (setq-hook! 'haskell-mode-hook +format-with 'ormolu)
  (setq haskell-interactive-popup-errors nil)
  (defun haskell-hoogle-lookup ()
    (interactive)
    (haskell-hoogle-start-server)
    (haskell-hoogle-lookup-from-local))
  (map! :localleader
        :map haskell-mode-map
        "l" #'haskell-hoogle-lookup))

(after! raku-mode
  (setq! raku-indent-offset tab-width)
  (add-hook 'raku-mode-hook (lambda () (set-input-method "TeX")))
  (add-hook 'raku-repl-mode-hook (lambda () (set-input-method "TeX")))
  (advice-add #'run-raku :filter-return (lambda (win) (window-buffer win))))

(after! rust-mode
  (defvar g/rustic-cargo-3ds nil
    "Set as t if we are in a cargo-3ds project.")
  (defun g/rustic-compilation-start-around (fn command &optional args)
    (funcall fn
             (if (and g/rustic-cargo-3ds (consp command))
                 (cons (car command) (cons "3ds" (cdr command)))
               command)
             args))
  (defun g/rustic-cargo-doc ()
    "Open the documentation for the current project in a browser.
The documentation is built if necessary."
    (interactive)
    (if (y-or-n-p "Open docs for dependencies as well?")
        ;; open docs only works with synchronous process
        (if g/rustic-cargo-3ds
            (shell-command (format "%s 3ds doc --open" (rustic-cargo-bin)))
          (shell-command (format "%s doc --open" (rustic-cargo-bin))))
      (if g/rustic-cargo-3ds
          (shell-command (format "%s 3ds doc --open --no-deps" (rustic-cargo-bin)))
        (shell-command (format "%s doc --open --no-deps" (rustic-cargo-bin))))))
  (advice-add #'rustic-cargo-doc :override #'g/rustic-cargo-doc)
  (advice-add #'rustic-compilation-start :around #'g/rustic-compilation-start-around)
  (setq rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(after! lua-mode
  (let ((lua-language-server-path
         (f-dirname (f-dirname (s-trim (shell-command-to-string "readlink -f `which lua-language-server`"))))))
    (setq lsp-clients-lua-language-server-bin (f-join lua-language-server-path "bin" "lua-language-server")
          lsp-clients-lua-language-server-main-location (f-join lua-language-server-path "share" "lua-language-server" "main.lua"))))

(after! fennel-mode
  (define-format-all-formatter fnlfmt
                               (:executable "fnlfmt")
                               (:install "nix profile install 'nixpkgs#fnlfmt'")
                               (:modes fennel-mode)
                               (:format (format-all--buffer-easy executable "-"))))

(after! lisp
  (setq! inferior-lisp-program "common-lisp.sh"))

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
  (add-to-list 'lispy-no-indent-modes 'minibuffer-mode)
  (add-to-list 'lispy-colon-no-space-regex '(fennel-mode . "")))

(setq-default tab-width 2)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(after! evil
  (evil-escape-mode -1))
;; (setq! ivy-posframe-style 'frame-top-center)

(defun insert-kbd-macro-in-register (register)
  "insert macro from register. prompt for register key
if no argument passed. you may need to revise inserted s-expression."
  (interactive "cthe register key:")
  (let* ((macro (cdr (assoc register register-alist)))
         (macro-string (with-temp-buffer
                         (setf last-kbd-macro macro)
                         (insert-kbd-macro '##)
                         (buffer-string))))
    (insert macro-string)))

(after! flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc haskell-stack-ghc))
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
        "https://www.youtube.com/feeds/videos.xml?channel_id=UC3ts8coMP645hZw9JSD3pqQ" ; andreas kling
        "https://www.youtube.com/feeds/videos.xml?channel_id=UCvVnz4iBRxBtVI2oe80S3kg" ; mhayashi
        "https://factorio.com/blog/rss"
        "https://xkcd.com/rss.xml"))
(after! elfeed
  (add-hook 'elfeed-search-mode-hook #'elfeed-update))

(use-package! hackernews)
(use-package! egg-timer)
(use-package! saveplace-pdf-view)
(use-package! disk-usage)
(use-package! sdcv-mode)

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
  (doom-completing-read-org-headings "Task: " "/home/gavin/docs/my/turns.org" :depth 1 :action (-cut update-turns--modify name <> <>)))

(load-file "~/.config/doom/project-specific/blog.el")

(setq load-prefer-newer t)              ; use while we are doing org-mode development
