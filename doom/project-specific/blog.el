;;; $DOOMDIR/project-specific/blog.el -*- lexical-binding: t; -*-

(require 's)
(require 'f)
(require 'request)

(defun g/blog--normalize-card-name (card-name)
  (s-downcase (s-replace " " "-" card-name)))

(defun g/blog--card-template (norm-card-name)
  (concat "{% include card.html card=\"" norm-card-name "\" %}"))

(defun g/blog-insert-card-reference ()
  (interactive)
  (let* ((file (f-join (projectile-project-root) "_data" "cards.json"))
         (json (with-temp-buffer
                 (insert-file-contents file)
                 (json-read))))
    (let ((card-name (ivy-read "Pick a card: " (-map #'car json))))
      (let* ((norm (g/blog--normalize-card-name card-name))
             (entry (alist-get (intern norm) json)))
        (if entry
            (insert (g/blog--card-template norm))
          (request
            "https://api.scryfall.com/cards/named"
            :params `(("fuzzy" . ,card-name))
            :parser 'json-read
            :sync t
            :timeout 1
            :error (cl-function (lambda (&key &allow-other-keys)
                                  (error "Failed to fetch data from scryfall.")))
            :success (cl-function (lambda (&key data &allow-other-keys)
                                    (let* ((url (alist-get 'scryfall_uri data))
                                           (name (alist-get 'name data))
                                           (image (or (alist-get 'small (alist-get 'image_uris data))
                                                      (alist-get 'small (alist-get 'image_uris (aref (alist-get 'card_faces data) 0)))))
                                           (norm (g/blog--normalize-card-name name)))
                                      (if (and url name image)
                                          (progn
                                            (push `(,(intern norm) (name . ,name) (url . ,url) (img . ,image)) json)
                                            (with-temp-buffer
                                              (insert (json-encode json))
                                              (write-region nil nil file))
                                            (insert (g/blog--card-template norm)))
                                        (error "Recved missing args from scryfall."))))))
          nil)))))

(defvar g/blog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a") #'g/blog-insert-card-reference)
    map))

(define-minor-mode g/blog-mode
  "Minor mode for my blog"
  :keymap g/blog-mode-map)
