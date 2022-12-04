#!/usr/bin/clisp
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-when (pred l &optional acc)
  (when (null l) (return-from split-when (list (reverse acc))))
  (if (funcall pred (car l))
    (cons (reverse acc) (split-when pred (cdr l)))
    (split-when pred (cdr l) (cons (car l) acc))))

(defun get-weight-per-elf (l)
  (mapcar
    (lambda (x) (reduce '+ (mapcar #'parse-integer x)))
    (split-when (lambda (x) (= (length x) 0)) l)))

(defun part-1 (l)
  (apply #'max (get-weight-per-elf l)))

(defun part-2 (l)
  (reduce '+ (subseq (reverse (sort (get-weight-per-elf l) '<)) 0 3)))

(format t "# PART 1 # Expected Result: ~d~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
