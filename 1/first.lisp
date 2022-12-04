#!/usr/bin/clisp
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun get-weight-per-elf (l)
  (reduce (lambda (acc val) (if (= (length val) 0)
                              (cons 0 acc)
                              (cons (+ (first acc) (parse-integer val)) (rest acc))))
          l
          :initial-value '(0)))

(defun part-1 (l)
  (apply #'max (get-weight-per-elf l)))

(defun part-2 (l)
  (reduce '+ (subseq (reverse (sort (get-weight-per-elf l) '<)) 0 3)))

(format t "# PART 1 # Expected Result: ~d~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
