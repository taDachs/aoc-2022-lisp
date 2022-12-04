#!/usr/bin/clisp
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-rucksack (l)
  (let ((size (/ (length l) 2)))
    (list (subseq l 0 size) (subseq l size))))

(defun find-shared (l)
  (let ((h (car l))
        (r (cdr l)))
    (loop for c across h
          do (when (every (lambda (x) (find c x)) r)
               (return-from find-shared c)))))

(defun get-priority (x)
  (let ((c (char-code x))
        (lower-a (char-code #\a))
        (upper-a (char-code #\A)))
    (if (>= c lower-a)
      (+ (- c lower-a) 1)
      (+ (- c upper-a) 27))))

(defun group-elves (l)
  (loop for i from 0 below (length l) by 3
        collect (subseq l i (+ i 3))))

(defun part-1 (l)
  (reduce '+ (mapcar (lambda (x) (get-priority (find-shared (split-rucksack x)))) l)))

(defun part-2 (l)
  (reduce '+ (mapcar (lambda (x) (get-priority (find-shared x))) (group-elves l))))

(format t "# PART 1 # Expected Result: ~d~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
