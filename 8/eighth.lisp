#!/usr/bin/clisp
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun is-visible (f x y)
  (let* ((row (nth y f))
         (col (mapcar (lambda (e) (nth x e)) f))
         (b (nth x (nth y f))))
    (some
      (lambda (x) (every (lambda (y) (< y b)) x))
      (list (subseq row 0 x) (reverse (subseq row (+ x 1)))
            (subseq col 0 y) (reverse (subseq col (+ y 1)))))))

(defun take-while-inc (p l)
  (when (null l) (return-from take-while-inc nil))
  (if (funcall p (first l))
    (cons (first l) (take-while-inc p (rest l)))
    (list (first l))))

(defun get-view-distance (b l)
  (length (take-while-inc (lambda (x) (< x b)) l)))

(defun calc-scenic-score (f x y)
  (let* ((row (nth y f))
         (col (mapcar (lambda (e) (nth x e)) f))
         (b (nth x (nth y f))))
    (reduce '*
            (mapcar (lambda (x) (get-view-distance b x))
                    (list (reverse (subseq row 0 x)) (subseq row (+ x 1))
                          (reverse (subseq col 0 y)) (subseq col (+ y 1)))))))

(defun get-indices (w h)
  (reduce #'append (loop for x from 0 below w
                         collect (loop for y from 0 below h
                                       collect (list x y)))))

(defun read-in-forest (l)
  (mapcar
    (lambda (x) (map 'list (lambda (y) (- (char-code y) (char-code #\0))) x))
    l))

(defun part-1 (l)
  (let* ((forest (read-in-forest l))
         (indices (get-indices (length (first forest)) (length forest))))
    (count-if #'identity (mapcar (lambda (x) (is-visible forest (first x) (second x))) indices))))

(defun part-2 (l)
  (let* ((forest (read-in-forest l))
         (indices (get-indices (length (first forest)) (length forest))))
    (reduce #'max (mapcar (lambda (x) (calc-scenic-score forest (first x) (second x))) indices))))

(format t "# PART 1 # Expected Result: ~a~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
