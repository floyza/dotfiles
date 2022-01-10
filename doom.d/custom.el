(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((lsp-haskell-formatting-provider . "fourmolu")
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
