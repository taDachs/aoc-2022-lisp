#!/usr/bin/clisp
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun all-diff (l)
  (when (null l) (return-from all-diff t))
  (and (every (lambda (x) (not (equal x (first l)))) (rest l))
       (all-diff (rest l))))

(defun find-first-marker (l n)
  (loop for i from 0 to (- (length l) n)
        do (when (all-diff (subseq l i (+ i n))) (return-from find-first-marker (+ i n)))))

(defun part-1 (l)
  (find-first-marker (map 'list #'identity (first l)) 4))

(defun part-2 (l)
  (find-first-marker (map 'list #'identity (first l)) 14))

(format t "# PART 1 # Expected Result: ~a~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
