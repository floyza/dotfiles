(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
   '("/home/gavin/docs/my/org/birthdays.org" "/home/gavin/docs/my/org/inbox.org" "/home/gavin/docs/my/org/plan.org" "/home/gavin/docs/my/org/someday.org" "/home/gavin/docs/my/org/tickler.org"))
 '(safe-local-variable-values
   '((lsp-file-watch-ignored-directories-additional "[////]Build\\'" "[////]Ports\\'" "[////]Toolchain/Local\\'" "[////]Toolchain/Tarballs\\'" "[////]Toolchain/Build\\'")
     (eval
      (lambda nil
        (when
            (and buffer-file-name
                 (string-match-p "\\.jack\\'" buffer-file-name)
                 (not
                  (string= major-mode "jack-mode")))
          (progn
            (message default-directory)
            (load "/home/gavin/docs/my/courses/nand2tetris/editor/jack-mode.el")
            (jack-mode)))))
     (eval
      (lambda nil
        (when
            (and buffer-file-name
                 (string-match-p "\\.jack\\'" buffer-file-name)
                 (not
                  (string= major-mode "jack-mode")))
          (progn
            (load "/home/gavin/my/courses/nand2tetris/editor/jack-mode.el")
            (jack-mode)))))
     (indent-tabs-mode 1)
     (g/rustic-cargo-3ds . t)
     (eval my-buffer-local-set-key
           (kbd "<normal-state> SPC m r")
           (lambda nil "Reflex-specific run project"
             (interactive)
             (haskell-process-restart)
             (haskell-process-load-file)
             (haskell-process-queue-without-filters
              (haskell-commands-process)
              "main")))
     (eval
      (lambda nil
        (when
            (and buffer-file-name
                 (string-match-p "\\.jack\\'" buffer-file-name)
                 (not
                  (string= major-mode "jack-mode")))
          (progn
            (load "/home/gavin/docs/my/courses/nand2tetris/editor/jack-mode.el")
            (jack-mode)))))
     (eval
      (lambda nil
        (when
            (and
             (string-match-p "\\.jack\\'" buffer-file-name)
             (not
              (string= major-mode "jack-mode")))
          (progn
            (load "/home/gavin/docs/my/courses/nand2tetris/editor/jack-mode.el")
            (jack-mode)))))
     (eval
      (lambda nil
        (when
            (and
             (string-match-p "\\.jack\\'" buffer-file-name)
             (not
              (string= major-mode "prog-mode")))
          (prog-mode))))
     (lsp-haskell-formatting-provider . "fourmolu")
     (eval setq-local org-roam-db-location
           (concat
            (locate-dominating-file default-directory ".dir-locals.el")
            "org-roam.db"))
     (eval setq-local org-roam-directory
           (locate-dominating-file default-directory ".dir-locals.el"))
     (org-roam-directory locate-dominating-file default-directory ".dir-locals.el")
     (haskell-process-type quote stack-ghci)
     (haskell-compiler-type quote stack)
     (eval c-set-offset 'inlambda 0)
     (eval c-set-offset 'access-label '-)
     (eval c-set-offset 'substatement-open 0)
     (eval c-set-offset 'arglist-cont-nonempty '+)
     (eval c-set-offset 'arglist-cont 0)
     (eval c-set-offset 'arglist-intro '+)
     (eval c-set-offset 'inline-open 0)
     (eval c-set-offset 'defun-open 0)
     (eval c-set-offset 'innamespace 0)
     (indicate-empty-lines . t)
     (c-block-comment-prefix . "  "))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
