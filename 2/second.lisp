#!/usr/bin/clisp
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun map-line (s lhs rhs)
  (list (nth (- (char-code (char s 0)) (char-code #\A)) lhs)
        (nth (- (char-code (char s 2)) (char-code #\X)) rhs)))

(defun get-required-move (you needed)
  (case needed
    ('lose (case you
             ('rock 'scissor)
             ('paper 'rock)
             ('scissor 'paper)))
    ('draw you)
    ('win  (case you
             ('rock 'paper)
             ('paper 'scissor)
             ('scissor 'rock)))))

(defun points-per-round (you me)
  (+ (cond ((equal you me) 3)
           ((case me
              ('rock (equal you 'scissor))
              ('paper (equal you 'rock))
              ('scissor (equal you 'paper))) 6)
           (t 0))
     (case me
       ('rock 1)
       ('paper 2)
       ('scissor 3))))

(defun get-exp-result (l)
  (reduce '+ (mapcar (lambda (x) (apply #'points-per-round x)) l)))

(defun part-1 (l)
  (get-exp-result
    (mapcar (lambda (x) (map-line x '(rock paper scissor) '(rock paper scissor))) l)))

(defun part-2 (l)
  (get-exp-result
    (mapcar (lambda (x) (let ((r (map-line x '(rock paper scissor) '(lose draw win))))
                          (list (first r) (apply #'get-required-move r))))
            l)))

(format t "# PART 1 # Expected Result: ~d~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
