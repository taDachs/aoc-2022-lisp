#!/usr/bin/clisp

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-string (c s)
  (let ((size (position c s)))
    (list (subseq s 0 size) (subseq s (+ size 1)))))

(defun parse-input (l)
  (let ((seg (loop for x in l while (> (length x) 0) collect x)))
    (list (transpose (mapcar #'do-the-split (subseq seg 0 (- (length seg) 1))))
          (mapcar #'parse-move-to-tuple (subseq l (+ (length seg) 1))))))

(defun do-the-split (l)
  (if (= (length l) 0)
    nil
    (let ((head (subseq l 0 (min 4 (length l))))
          (tail (do-the-split (subseq l (min 4 (length l))))))
      (cons (if (every (lambda (x) (equal x #\SPACE)) head) nil (char head 1)) tail))))

(defun transpose (l)
  (let ((size (apply #'max (mapcar #'length l))))
    (loop for i from 0 below size
          collect (remove-if #'null (mapcar (lambda (x) (nth i x)) l)))))

(defun parse-move-to-tuple (l)
  ;; should use regex for that
  (let* ((w/o-move (subseq l 5))
         (a1 (split-string #\SPACE w/o-move))
         (num (first a1))
         (w/o-from (subseq (second a1) 5))
         (a2 (split-string #\SPACE w/o-from))
         (from (first a2))
         (to (subseq (second a2) 3)))
    (mapcar #'parse-integer (list num from to))))

(defun pop-container (from l n)
  (if (= from 1)
    (let ((h (first l)))
      (list (subseq h 0 n) (cons (subseq h n) (rest l))))
    (let* ((res (pop-container (- from 1) (rest l) n))
           (c (first res))
           (ll (second res)))
      (list c (cons (first l) ll)))))

(defun place-container (to l c)
  (if (= to 1)
    (let ((h (first l)))
      (cons (append c h) (rest l)))
    (cons (first l) (place-container (- to 1) (rest l) c))))

(defun move-container-single (n from to l)
  (let* ((out (pop-container from l 1))
         (c (first out))
         (ll (second out))
         (res (place-container to ll c)))
    (if (= n 1)
      res
      (move-container-single (- n 1) from to res))))

(defun move-container-set (n from to l)
  (let* ((out (pop-container from l n))
         (c (first out))
         (ll (second out)))
    (place-container to ll c)))

(defun part-1 (l)
  (let* ((input (parse-input l))
         (final-stack (reduce (lambda (x y) (move-container-single (first y) (second y) (third y) x))
                              (second input)
                              :initial-value (first input))))
    (concatenate 'string (mapcar #'first final-stack))))

(defun part-2 (l)
  (let* ((input (parse-input l))
         (final-stack (reduce (lambda (x y) (move-container-set (first y) (second y) (third y) x))
                              (second input)
                              :initial-value (first input))))
    (concatenate 'string (mapcar #'first final-stack))))


(format t "# PART 1 # Expected Result: ~a~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
