#!/usr/bin/clisp

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-string (c s)
  (let ((size (position c s)))
    (list (subseq s 0 size) (subseq s (+ size 1)))))

(defun string-to-tuple (s)
  (mapcar (lambda (x) (sort (mapcar #'parse-integer (split-string #\- x)) '<))
          (split-string #\, s)))

(defun does-overlap-fully (l r)
  (let ((l-min (first l)) (l-max (second l)) (r-min (first r)) (r-max (second r)))
    (or (and (>= l-min r-min) (<= l-max r-max))
        (and (<= l-min r-min) (>= l-max r-max)))))

(defun does-overlap-partly (l r)
  (let ((l-min (first l)) (l-max (second l)) (r-min (first r)) (r-max (second r)))
    (or (and (<= l-min r-min) (<= r-min l-max))
        (and (<= r-min l-min) (<= l-min r-max)))))

(defun part-1 (l)
  (count-if (lambda (x) (apply #'does-overlap-fully (string-to-tuple x))) l))

(defun part-2 (l)
  (count-if (lambda (x) (apply #'does-overlap-partly (string-to-tuple x))) l))

(format t "# PART 1 # Expected Result: ~d~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
