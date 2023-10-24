;; -*- lexical-binding: t -*-
(require 'subr-x)
(defun hypergeometric (black-balls total-balls sample-size wanted-count pred)
  (when (> black-balls total-balls)
    (error "Black balls cannot be larger than total-balls"))
  (funcall (let (rec)
             (setq rec (lambda (black-balls total-balls sample-size picked-count)
                         (if (or (<= black-balls 0) (<= total-balls 0) (<= sample-size 0))
                             (if (funcall pred picked-count wanted-count)
                                 1
                               0)
                           (+ (* (funcall rec
                                          (1- black-balls)
                                          (1- total-balls)
                                          (1- sample-size)
                                          (1+ picked-count))
                                 (/ (float black-balls) total-balls))
                              (* (funcall rec
                                          black-balls
                                          (1- total-balls)
                                          (1- sample-size)
                                          picked-count)
                                 (- 1 (/ (float black-balls) total-balls)))))))
             rec)
           black-balls
           total-balls
           sample-size
           0))

(hypergeometric 4 60 7 1 #'>=)
