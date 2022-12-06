#!/usr/bin/clisp

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-string-on-space (s &optional acc)
  (reverse (mapcar (lambda (x) (concatenate 'string (reverse x)))
                   (reduce (lambda (acc e) (if (equal e #\SPACE)
                                             (cons nil acc)
                                             (cons (cons e (first acc)) (rest acc))))
                           s
                           :initial-value nil))))

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
  (let ((split (split-string-on-space l)))
    (mapcar #'parse-integer (list (second split) (fourth split) (sixth split)))))

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

(defun move-container (l m &optional rev)
  (let ((out (pop-container (second m) l (first m))))
    (place-container (third m) (second out) (if rev (reverse (first out)) (first out)))))

(defun part-1 (l)
  (let* ((input (parse-input l))
         (final-stack (reduce (lambda (x y) (move-container x y t)) (second input) :initial-value (first input))))
    (concatenate 'string (mapcar #'first final-stack))))

(defun part-2 (l)
  (let* ((input (parse-input l))
         (final-stack (reduce #'move-container (second input) :initial-value (first input))))
    (concatenate 'string (mapcar #'first final-stack))))

(format t "# PART 1 # Expected Result: ~a~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
