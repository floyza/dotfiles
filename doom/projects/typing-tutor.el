;;; typing-tutor.el -*- lexical-binding: t; -*-

(defvar-keymap typing-tutor-mode-map
  :full t
  :suppress 'nodigits
  :doc "Keymap for typing-tutor-mode"
  "0" #'typing-tutor-insert-number
  "1" #'typing-tutor-insert-number
  "2" #'typing-tutor-insert-number
  "3" #'typing-tutor-insert-number
  "4" #'typing-tutor-insert-number
  "5" #'typing-tutor-insert-number
  "6" #'typing-tutor-insert-number
  "7" #'typing-tutor-insert-number
  "8" #'typing-tutor-insert-number
  "9" #'typing-tutor-insert-number)

(defun typing-tutor-insert-number ()
  (interactive))
